// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::plugins(cpp11)]]
#include "config.h"                     // for valgrind, CXX11 etc
#include "local.h"                     // for ICD_OPENMP
#include "icd_types.h"                 // for ComorbidOut, VecVecInt, VecVec...
#include "comorbidMatMul.h"
#include "comorbidCommon.h"
#include "comorbidSetup.h"
#include "fastIntToString.h"
#include <algorithm>                   // for binary_search, copy
#include <vector>                      // for vector, vector<>::const_iterator
#include <unordered_set>
#include "util.h"                     // for debug_parallel
#include <string>
#include <cstring>
extern "C" {
#include "cutil.h"                              // for getRListOrDfElement
}

/*
 MAKEFLAGS=-j16 R -e 'devtools::load_all(); icd9_comorbid_ahrq(ahrq_test_dat)'
 MAKEFLAGS=-j16 R -e 'devtools::load_all(); test(reporter="Location")'
 MAKEFLAGS=-j16 R -e 'devtools::load_all(); mydf <- data.frame(visit_id = c("a", "a"), icd9 = c("441", "412.93")); icd9_comorbid_quan_deyo(mydf)'
 */

using namespace Rcpp;

class Relevant {
public:
  const List& src_map;
  const CV relevant;
  IHS hash;
  CV keys;
  Relevant(const List& map, const CV& codes) : src_map(map), relevant(findRelevant(codes)), hash(IHS(relevant).fill()), keys(hash.keys()) {}

  CV findRelevant(const CV& codes) {
    std::unordered_set<std::string> r;
    r.reserve(100 * src_map.size());
    IHS codeHash(codes);
    codeHash.fill();
    DEBUG_VEC(codes);
    DEBUG_VEC(codeHash.keys());
    // TODO codes may be factor, if so, return levels
    for (CV cmb : src_map) {
      for (auto code : cmb) {
        // IndexHash contains causes compiler warning, so compare directly:
        if ((int) codeHash.get_index(code) != NA_INTEGER) {
          TRACE("Pushing back " << code);
          //r.push_back(((String) code).get_cstring());
          r.insert(((String) code).get_cstring());
        }
      }
    }
    return(wrap(r)); // or keep as STL container?
  }

  R_xlen_t size() { return relevant.size(); }
}; // Relevant

// TODO: make exportable version of get relevant? // [[Rcpp::export]]

void printCornerMap(DenseMap x) {
  DEBUG("Map matrix:");
  if (x.rows() >= 8 && x.cols() >= 32) {
    DenseMap block = x.block<8, 32>(0, 0);
    DEBUG(block);
  } else if (x.rows() < 5 && x.cols() < 32) {
    DEBUG(x);
  } else if (x.rows() > 1 && x.cols() > 1) {
    DEBUG(x(0, 0) << ", " << x(0, 1) << ", " <<
      x(1, 0) << ", " << x(1, 1));
  } else if (x.rows() == 1 && x.cols() == 1) {
    DEBUG(x(0, 0));
  } else {
    DEBUG("map mat empty");
  }
  DEBUG("Map matrix rows: " << x.rows() << ", and cols: " << x.cols());
}

void printCornerSparse(PtsSparse x) {
  DEBUG("converting visMat to dense for debugging only:");
  Eigen::MatrixXi dense = Eigen::MatrixXi(x);
  if (x.rows() >= 4 && x.cols() >= 4)
    Rcpp::Rcout << dense.block<4, 4>(0, 0) << std::endl;
  else
    Rcpp::Rcout << dense << std::endl;
}

#ifdef ICD_DEBUG
#define PRINTCORNERMAP(x) Rcpp::Rcout << #x << ": "; printCornerMap(x);
#define PRINTCORNERSP(x) Rcpp::Rcout << #x << ": "; printCornerSparse(x);
#define ICD_ASSIGN(row,col) mat(row, col) = true; // bounds check
#else
#define PRINTCORNERMAP(x) ((void)0);
#define PRINTCORNERSP(x) ((void)0);
#define ICD_ASSIGN(row,col) mat.coeffRef(row, col) = true;
#endif

// This is a complicated function
//
// Goal is to avoid any string matching or even hash calculation at all.
//
// visitids are easier, because we need to build an IndexHash for them, but we
// have to do this in a type specific way. int, numeric, string, factor...
//
// icd codes are either factor or character type. if a factor, we can use the
// factor level index as the column in the visit matrix, but we need to know
// this also to construct the comorbidity matrix where the codes are rows.
//
// If the icd codes are character, we can construct the indexhash, then return a
// new factor. This is basically what factorNosort does, but I need the hash
// internals for lookups, so can't use it directly here.
void buildVisitCodesSparseSimple(const RObject& visits,
                                 const RObject& codes, // todo handle factor in parent function
                                 Relevant& rh,
                                 PtsSparse& visMat,
                                 VecStr& visitIds // get this from sparse matrix at end, but needed?
) {
  TRACE("Starting build of visit matrix");
  assert(Rf_length(visits) == Rf_length(codes));
  const CV relevantKeys = rh.keys;
  const R_xlen_t relevantSize = rh.size();
  R_xlen_t vlen = Rf_length(visits);
  DEBUG("Setting levels");
  DEBUG("relevantSize = " << relevantSize << ", vlen = " << vlen);
  std::vector<Triplet> visTriplets;
  visTriplets.reserve(vlen);
  IntegerVector rows, cols;
  if (!Rf_isFactor(codes)) {
    DEBUG("codes are still character...");
    const CV& codes_cv = (CV) codes;
    DEBUG_VEC(codes_cv);
    DEBUG_VEC(relevantKeys);
    DEBUG("rh.keys.size() = " << rh.keys.size());
    cols = rh.hash.lookup(codes_cv); // C indexed
  } else {
    DEBUG("codes are still in a factor...");
    CV code_levels = wrap(codes.attr("levels"));
    const IntegerVector codes_relevant =
      refactor((IntegerVector) codes, rh.relevant, true); // no NA in output levels, please
    //const CV code_levels_cv = (CV) code_levels;
    //assert(Rf_length(code_relevant.attr("levels")) == relevantSize);
    //assert(code_levels_cv[0] == relevantKeys[0]); // TODO better checks?
    cols = ((IntegerVector) codes_relevant); // keep R indexing
  }
  if (!Rf_isFactor(visits)) {
    CV v = (CV) visits; // assume character for now
    DEBUG("visits are not factors, they are: "
            << TYPEOF(visits)
            << " but converting/assuming character. "
            << "See https://cran.r-project.org/doc/manuals/r-release/R-ints.html#SEXPs");
    DEBUG_VEC(v);
    CV uv = unique(v);
    DEBUG_VEC(uv);
    // with current Rcpp (June 1, 2018), there is no way to avoid doing two
    // hashes, one to get unique keys, and one to match them. Quick tests show
    // this adds about 10% time. If the hash caching mechanism actually worked,
    // it wouldn't be a problem.
    visitIds = as<VecStr>(uv);
    DEBUG_VEC(visitIds);
    // cannot do this:
    // IHS visHash = IHS(uv);
    //rows = visHash.lookup(uv.get_ref()); // it should work by
    // using pointer to global string cache (another hash!), but it appears not
    // to do this.
    rows = match(v, uv);
    // matrix row names are character. TODO: consider option not to
    // name the rows, e.g. for people who just summarize the data: will save a
    // lot of character processing
  } else {
    DEBUG("visits are factors");
    // CV visit_levels = wrap(visits.attr("levels"));
    //  use the factor level as the row number (R vs C index)
    //rows = ((IntegerVector) visits - 1);
    // keep R index
    rows = visits; // can do this without copy using unique_ptr?
    visitIds = as<VecStr>(rows.attr("levels"));
  }
  DEBUG_VEC(rows);
  DEBUG_VEC(cols);
  DEBUG("n rows: " << rows.size() << ", n cols: " << cols.size());
  assert(rows.size() == cols.size());
  visMat.resize(visitIds.size(), relevantSize); // unique ids
  visMat.reserve(vlen); // number of triplets is just vlen (for long data)

  // now we have rows and columns, just make the triplets and insert.
  for (R_xlen_t i = 0; i != rows.size(); ++i) {
    if (IntegerVector::is_na(cols[i])) {
     TRACE("dropping NA col value");
    continue;
    }
    TRACE("adding triplet at R idx:" << rows[i] << ", " << cols[i]);
    visTriplets.push_back(Triplet(rows[i] - 1, cols[i] - 1, true));
  }
  TRACE("visMat rows: " << visMat.rows() << ", cols: " << visMat.cols());
  TRACE("visMat to be updated, setting visMat from triplets...");
  TRACE("there are " << visTriplets.size() << " triplets");
  visMat.setFromTriplets(visTriplets.begin(), visTriplets.end());
  visMat.conservativeResize(visitIds.size(), relevantSize);
  DEBUG("Built visMt, rows:" << visMat.rows() << ", cols: " << visMat.cols());
  PRINTCORNERSP(visMat);
}

void buildVisitCodesSparse(const SEXP& icd9df,
                           const std::string& visitId,
                           const std::string& icd9Field,
                           PtsSparse& visit_codes_sparse,
                           VecStr& visitIds // will have to get this from sparse matrix at end, but needed?
) {
  SEXP icds = PROTECT(getRListOrDfElement(icd9df, icd9Field.c_str())); // this is a factor
  SEXP vsexp = PROTECT(getRListOrDfElement(icd9df, visitId.c_str()));
  IntegerVector icd9dfFactor = as<IntegerVector>(icds);
  CV factorLevels = icd9dfFactor.attr("levels");
  R_xlen_t numUniqueCodes = factorLevels.length();

  //char* lastVisit;
  //std::strcpy(lastVisit, "JJ94967295JJ"); // random string
  std::string lastVisit = "JJ94967295JJ94967295JJ"; // random long string TODO: test consequences of going over length.

  int vlen = Rf_length(icds); // same as length of vsexp
  // make an unordered set for quick check for duplicates while building list of unique visit ids
  VisLk vis_lookup;
  vis_lookup.reserve(vlen);
  // also maintain list of (ordered as first encountered) visit ids
  visitIds.resize(vlen); // resize and trim at end, as alternative to reserve
  int n;

  VecVecIntSz vcdb_max_idx = -1; // we increment immediately to zero as first index
  VecVecIntSz vcdb_new_idx;
  VecVecIntSz vcdb_last_idx = 2094967295; // extremely unlikely random, < 2^32 (to avoid 32bit R build warn)

  visit_codes_sparse.resize(vlen, numUniqueCodes); // overestimate badly to start
  visit_codes_sparse.reserve(vlen); // but memory commitment is known and limited.

  std::vector<Triplet> visTriplets;
  visTriplets.reserve(vlen * 30); // overestimate codes per patient to avoid resizing while filling

  // the result matrix size should have dimensions rows: number of unique
  // visitIds, cols: number of unique ICD codes.
  for (int i = 0; i != vlen; ++i) {
#ifdef ICD_DEBUG_SETUP_TRACE
    Rcpp::Rcout << "vcdb_max_idx: " << vcdb_max_idx <<
      " vcdb_new_idx: " << vcdb_new_idx <<
        " vcdb_last_idx: " <<  vcdb_last_idx << std::endl;
#endif
    std::string visit = CHAR(STRING_ELT(vsexp, i));
    n = INTEGER(icds)[i]; // ICD codes are in a factor, so get the integer index

    if (lastVisit != visit) {
#ifdef ICD_DEBUG_SETUP_TRACE
      Rcpp::Rcout << "visit has changed" << std::endl;
#endif
      vcdb_new_idx = vcdb_max_idx + 1;
      VisLk::iterator found = vis_lookup.find(visit); // did we see this visit already? Get name-index pair.
      if (found != vis_lookup.end()) { // we found the old visit
        // we saved the index in the map, so use that to insert a triplet:
#ifdef ICD_DEBUG_SETUP
        Rcpp::Rcout << "Found " << found->first << " with row id: " << found->second << std::endl;
#endif
#ifdef ICD_DEBUG_SETUP_TRACE
        Rcpp::Rcout << "adding true at index (" << found->second << ", " << n-1 << ")" << std::endl;
#endif
        visTriplets.push_back(Triplet(found->second, n - 1, true));
        continue; // and continue with next row
      } else { // otherwise we found a new visitId, so add it to our lookup table
#ifdef ICD_DEBUG_SETUP_TRACE
        Rcpp::Rcout << "visit is new" << std::endl;
#endif
        VisLkPair vis_lookup_pair = std::make_pair(visit, vcdb_new_idx);
        vis_lookup.insert(vis_lookup_pair); // new visit, with associated position in vcdb
      }
      // we didn't find an existing visitId
#ifdef ICD_DEBUG_SETUP_TRACE
      Rcpp::Rcout << "adding true at index (" << vcdb_new_idx << ", " << n-1 << ")" << std::endl;
#endif
      visTriplets.push_back(Triplet(vcdb_new_idx, n - 1, true));
      visitIds[vcdb_new_idx] = visit; // keep list of visitIds in order encountered.
      lastVisit = visit;
      vcdb_last_idx = vcdb_new_idx;
      ++vcdb_max_idx;
    } else { // last visitId was the same as the current one, so we can skip all the logic
#ifdef ICD_DEBUG_SETUP_TRACE
      Rcpp::Rcout << "adding true at index (" << vcdb_last_idx << ", " << n-1 << ")" << std::endl;
#endif

      visTriplets.push_back(Triplet(vcdb_last_idx, n - 1, true));
    }
  } // end loop through all visit-code input data
  UNPROTECT(2);

  // visit_codes_sparse and visitIds are updated
  visit_codes_sparse.setFromTriplets(visTriplets.begin(), visTriplets.end());
  visit_codes_sparse.conservativeResize(vcdb_max_idx + 1, numUniqueCodes);
  visitIds.resize(vcdb_max_idx + 1); // we over-sized (not just over-reserved) so now we trim.
}

class MapPlus {
public:
  MapPlus(const List& icd9Mapping, const Relevant& rh);
  void buildMatrix();
  List map; // consider ListOf<IntegerVector>
  DenseMap mat;
  R_xlen_t rows() { return mat.rows(); }
};
// constructor
MapPlus::MapPlus(const List& mapList, const Relevant& rh) {
  // take a map of character vectors or factors and reduce it to only relevant
  // codes using hashmap
  //
  // downside is that each list element has a copy of the same relevant levels.
  //List remap(const List& map, IHS& relevantHash) {
  CharacterVector cmbs = mapList.names();
  bool areFactors = Rf_isFactor(mapList[0]);
  for (R_xlen_t i = 0; i != mapList.size(); ++i) {
    String cmb_name = cmbs[i];
    TRACE("remapping: " << cmb_name.get_cstring());
    if (!areFactors && TYPEOF(mapList[0]) != STRSXP)
      Rcpp::stop("remap expects a list of only character vectors or factors.");
    if (areFactors) {
      TRACE("factor in input map");
      IntegerVector this_map_cmb = mapList[i];
      map[cmb_name] = refactor_narm(this_map_cmb, rh.keys);
    } else { // most common case (re-use the hash!)
      TRACE("character vector in input map");
      CV this_map_cmb = mapList[i];
      // make factor using existing hash, so R-indexed numbers.
      IntegerVector this_cmb = (IntegerVector) rh.hash.lookup(this_map_cmb);
      this_cmb.attr("levels") = (CharacterVector) rh.keys;
      this_cmb.attr("class") = "factor";
      this_cmb = this_cmb[!is_na(this_cmb)];
      TRACE_VEC(this_cmb);
      map[cmb_name] = this_cmb;
    } // factor or not
  } // for
  DEBUG("Map reduced. Initializing the Eigen matrix");
  mat = DenseMap(rh.keys.size(), mapList.size());
  mat.setZero();
  DEBUG("mat rows: " << mat.rows() << ", cols: " << mat.cols());
  buildMatrix();
  DEBUG("map matrix built");
}

// takes a map of _factors_ produced by remap. These already only contain
// relevant codes, with factors indicies being relevant.
void MapPlus::buildMatrix() {
  TRACE("map SEXP type is: " << TYPEOF(map[0]));
  assert(Rf_isFactor(map[0]));
  for (auto li = map.begin(); li != map.end(); ++li) {
    auto col = std::distance(map.begin(), li);
    TRACE("working on comorbidity: " << col);
    IntegerVector v(*li);
    for (R_xlen_t vi = 0; vi != v.size(); ++vi) {
      TRACE("cmb: vi=" << vi << " v[vi]=" << v[vi] << " col=" << col);
      if (!IntegerVector::is_na(v[vi])) {
        ICD_ASSIGN(v[vi] - 1, col); // R to C indexing: the factor index is row
      }
    }
  }
  PRINTCORNERMAP(mat);
}

//' @title Comorbidity calculation as a matrix multiplication
//' @description
//' The problem is that the matrices could be huge: the patient-icd matrix would
//' be millions of patient rows, and ~15000 columns for all AHRQ comorbidities.
//' @details
//' Several ways of reducing the problem: firstly, as with existing code, we can
//' drop any ICD codes from the map which are not in the patient data. With many
//' patients, this will be less effective as the long tail becomes apparent.
//' However, with the (small) Vermont data, we see ~15,000 codes being reduced to
//' 339.
//' @section Sparse matrices:
//' Using sparse matrices is another solution. Building
//' the initial matrix may become a significant part of the calculation, but once
//' done, the solution could be a simple matrix multiplication, which is
//' potentially highly optimized (Eigen, BLAS, GPU, etc.)
//' @section Eigen:
//' Eigen has parallel (non-GPU) optimized sparse row-major *
//' dense matrix. Patients-ICD matrix must be the row-major sparse one, so the
//' dense matrix is then the comorbidity map
//' \url{https://eigen.tuxfamily.org/dox/TopicMultiThreading.html}
//' @keywords internal array algebra
// [[Rcpp::export]]
LogicalMatrix comorbidMatMulSimple(const DataFrame& icd9df,
                                   const List& icd9Mapping,
                                   const std::string visitId,
                                   const std::string icd9Field) {
  valgrindCallgrindStart(false);
  VecStr out_row_names; // size is reserved in buildVisitCodesVec
  RObject visits = icd9df[visitId]; // does this copy??? RObject instead?
  CV codes = icd9df[icd9Field];
  //TODO: Relevant requires CV right now, not factor
  Relevant r(icd9Mapping, codes); // potential to template over codes type
  MapPlus m(icd9Mapping, r);
  PtsSparse visMat; // reservation and sizing done within next function
  DEBUG("*** building visMat ***");
  buildVisitCodesSparseSimple(visits, codes, r, visMat, out_row_names);
  DEBUG("built visit matrix");
  if (visMat.cols() != m.rows())
    Rcpp::stop("matrix multiplication won't work");
  DenseMap result = visMat * m.mat; // col major result
  DEBUG("Result rows: " << result.rows() << ", cols: " << result.cols());
  PRINTCORNERMAP(result);
  Rcpp::IntegerMatrix mat_out_int = Rcpp::wrap(result);
  Rcpp::LogicalMatrix mat_out_bool = Rcpp::wrap(mat_out_int);
  List dimnames = Rcpp::List::create(out_row_names, icd9Mapping.names());
  mat_out_bool.attr("dimnames") = dimnames;
  valgrindCallgrindStop();
  return mat_out_bool;
}

//' @title Comorbidity calculation as a matrix multiplication
//' @description
//' The problem is that the matrices could be huge: the patient-icd matrix would
//' be millions of patient rows, and ~15000 columns for all AHRQ comorbidities.
//' @details
//' Several ways of reducing the problem: firstly, as with existing code, we can
//' drop any ICD codes from the map which are not in the patient data. With many
//' patients, this will be less effective as the long tail becomes apparent.
//' However, with the (small) Vermont data, we see ~15,000 codes being reduced to
//' 339.
//' @section Sparse matrices:
//' Using sparse matrices is another solution. Building
//' the initial matrix may become a significant part of the calculation, but once
//' done, the solution could be a simple matrix multiplication, which is
//' potentially highly optimized (Eigen, BLAS, GPU, etc.)
//' @section Eigen:
//' Eigen has parallel (non-GPU) optimized sparse row-major *
//' dense matrix. Patients-ICD matrix must be the row-major sparse one, so the
//' dense matrix is then the comorbidity map
//' \url{https://eigen.tuxfamily.org/dox/TopicMultiThreading.html}
//' @keywords internal array algebra
// [[Rcpp::export]]
LogicalMatrix comorbidMatMul(const DataFrame& icd9df, const List& icd9Mapping,
                             const std::string visitId,
                             const std::string icd9Field,
                             const int threads = 8, const int chunk_size = 256,
                             const int omp_chunk_size = 1) {
  valgrindCallgrindStart(true);
  VecStr out_row_names; // size is reserved in buildVisitCodesVec
  // find eventual size of map matrix:
  size_t map_rows = 0; // count number of codes in each comorbidity (don't look for codes which fall in two categories...)
  for (List::const_iterator li = icd9Mapping.begin(); li != icd9Mapping.end(); ++li) {
    IntegerVector v = *li;
    map_rows += v.size();
  }
  // make an integer matrix for the map. not sparse. No boolean option, I don't
  // think. N.b. codes can appear in multiple categories, e.g. hypertension
  // secondary to kidney disease. For matrix multiple to work, it must be
  // possible to represent this. Since the ICD code in the patient data is
  // encoded as a factor (which cannot have duplicates), rows in the map with
  // identical ICD codes must be reduced to a single row containing two or more
  // cells set to  'true' for the relevant comorbidities associated with that
  // code.
  DenseMap map(map_rows, icd9Mapping.length());
  map.setZero();
  // fill this dense matrix in col major order (each column is a comorbidity),
  // and accumulate row index over multiple loops.
  R_xlen_t row = 0;
  typedef std::unordered_map<int, int> Lookup;
  Lookup map_lookup;
  Lookup::iterator found_it;
  map_lookup.reserve(map_rows); // overestimate slightly, most are not duplicated.
  for (List::const_iterator li = icd9Mapping.begin(); li != icd9Mapping.end(); ++li) {
    auto col = std::distance(icd9Mapping.begin(), li);
#ifdef ICD_DEBUG_SETUP_TRACE
    Rcpp::Rcout << "working on map item/col: " << col << std::endl;
#endif
    IntegerVector v = *li;
    for (IntegerVector::iterator vi = v.begin(); vi != v.end(); ++vi) {
#ifdef ICD_DEBUG_SETUP_TRACE
      Rcpp::Rcout << "working on vector item: " << std::distance(v.begin(), vi);
      Rcpp::Rcout << " with row: " << row << std::endl;
#endif
      int this_code_factor_number = *vi; // no need to cast
      // if we can't find the integer in the lookup, then we add row
      found_it = map_lookup.find(this_code_factor_number);
      if (found_it == map_lookup.end()) {
#ifdef ICD_DEBUG_SETUP_TRACE
        Rcpp::Rcout << "not found in lookup, so adding to map at " <<
          "row " << row << ", col " << col << std::endl;
#endif
#ifdef ICD_DEBUG
        map(row, col) = true;
#else
        map.coeffRef(row, col) = true; // coeffRef doesn't do bounds check
#endif
        map_lookup.insert(std::make_pair(this_code_factor_number, row));
        ++row;
      } else {
#ifdef ICD_DEBUG_SETUP_TRACE
        Rcpp::Rcout << "inserting duplicate while building map" <<
          " into row " << found_it->second << ", col " << col << std::endl;
#endif
#ifdef ICD_DEBUG
        map(found_it->second, col) = true;
#else
        map.coeffRef(found_it->second, col) = true;
#endif
        // do not update lookup, or increment row
      }
    }
  }
  map.conservativeResize(row, map.cols()); // conservative resize to avoid emptying the matrix!

#ifdef ICD_DEBUG_SETUP
  Rcpp::Rcout << "Map matrix:" << std::endl;
  if (map.rows() >= 5 && map.cols() >= 5)
    Rcpp::Rcout << map.block<5, 5>(0, 0) << std::endl;
  else
    Rcpp::Rcout << map << std::endl;
  // hmm map is sparser than the visit-icd codes, could have sparse map on left, and transposed visit-icd on right
  Rcpp::Rcout << "Map matrix rows: " <<
    map.rows() << ", and cols: " << map.cols() << std::endl;
#endif

  // build the patient:icd matrix... can probably re-use and simplify the
  PtsSparse visit_codes_sparse; // reservation and sizing done within next function
  buildVisitCodesSparse(icd9df, visitId, icd9Field, visit_codes_sparse, out_row_names);
#ifdef ICD_DEBUG_SETUP
  Rcpp::Rcout << "Built the sparse matrix, rows:" << visit_codes_sparse.rows() <<
    ", cols: " << visit_codes_sparse.cols() << std::endl;
#ifdef ICD_DEBUG_SETUP_SLOW
  // just for debugging, convert to dense to show contents:
  Rcpp::Rcout << "converting visit_codes_sparse to dense for debugging only (slow!):" << std::endl;
  Eigen::MatrixXi dense = Eigen::MatrixXi(visit_codes_sparse);
  Rcpp::Rcout << "visit_codes_sparse:" << std::endl;
  if (visit_codes_sparse.rows() >= 4 && visit_codes_sparse.cols() >= 4)
    Rcpp::Rcout << dense.block<4, 4>(0, 0) << std::endl;
  else
    Rcpp::Rcout << dense << std::endl;
#endif
#endif

  if (visit_codes_sparse.cols() != map.rows())
    Rcpp::stop("matrix multiplication won't work");

  DenseMap result = visit_codes_sparse * map; // col major result

#ifdef ICD_DEBUG_SETUP
  Rcpp::Rcout << " done matrix multiplication. Result has " <<
    "rows: " << result.rows() <<
      " and cols: " << result.cols() << std::endl;
  Rcpp::Rcout << "matrix result begins: " << std::endl;
  if (result.rows() >= 4 && result.cols() >= 4)
    Rcpp::Rcout << result.block<4, 4>(0, 0) << std::endl;
  else
    Rcpp::Rcout << result << std::endl;
#endif
#ifdef ICD_DEBUG_SETUP
  Eigen::Array<bool, Eigen::Dynamic, Eigen::Dynamic> result_bool = (result.array() != 0);
  Rcpp::Rcout << "Result boolean array has " <<
    "rows: " << result_bool.rows() <<
      " and cols: " << result_bool.cols() << std::endl;
  Rcpp::Rcout << "bool result begins: " << std::endl;
  if (result_bool.rows() >= 4 && result_bool.cols() >= 4)
    Rcpp::Rcout << result_bool.block<4, 4>(0, 0) << std::endl;
  else
    Rcpp::Rcout << result_bool << std::endl;
#endif
  // Rcpp::LogicalMatrix mat_out_bool = Rcpp::wrap(result); // segfaults sometimes
  Rcpp::IntegerMatrix mat_out_int = Rcpp::wrap(result); // try to avoid the segfault
  Rcpp::LogicalMatrix mat_out_bool = Rcpp::wrap(mat_out_int);
  List dimnames = Rcpp::List::create(out_row_names, icd9Mapping.names());
  CharacterVector rownames = dimnames[0];
  CharacterVector colnames = dimnames[1];
#ifdef ICD_DEBUG_SETUP
  Rcpp::Rcout << "mat_out rows = " << mat_out_bool.rows() << std::endl;
  Rcpp::Rcout << "mat_out cols = " << mat_out_bool.cols() << std::endl;
  Rcpp::Rcout << "Length of dimnames = " << dimnames.size() << std::endl;
  Rcpp::Rcout << "Length of rownames = " << rownames.size() << std::endl;
  Rcpp::Rcout << "Length of colnames = " << colnames.size() << std::endl;
#endif
  mat_out_bool.attr("dimnames") = dimnames;
#ifdef ICD_DEBUG_SETUP
  Rcpp::Rcout << "dimension names set" << std::endl;
#endif
  valgrindCallgrindStop();
  return mat_out_bool;
}
