// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::interfaces(r, cpp)]]
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

using namespace Rcpp;

// [[Rcpp::export]]
List remap(const List& map, const CharacterVector& relevant) {
  // TODO at this point, could switch to STL
  List out = List::create();
  CharacterVector cmbs = map.names(); // what if already a factor? TODO:
  for (R_xlen_t i = 0; i != map.size(); ++i) {
    String cmb = cmbs[i];
    out[cmb] = factorNoSort(map[cmb], relevant, true);
  }
  return(out);
}

// [[Rcpp::export]]
CV getRelevant(List map, CV codes) {
  VecStr relevant;
  relevant.reserve(100 * map.size());
  Rcpp::sugar::IndexHash<16> codeHash(codes);
  codeHash.fill();
  for (CV cmb : map) {
    for (auto code : cmb) {
      if (codeHash.contains(code))
        relevant.push_back(((String)code).get_cstring());
    }
  }
  return wrap(relevant);
}

void printCornerMap(DenseMap x) {
  DEBUG("Map matrix:");
  if (x.rows() >= 5 && x.cols() >= 5) {
    DenseMap block = x.block<5, 5>(0, 0);
    DEBUG(block);
  }
  else
    DEBUG(x);
  DEBUG("Map matrix rows: " << x.rows() << ", and cols: " << x.cols());
}

void printCornerSparse(PtsSparse visMat) {
  DEBUG("converting visMat to dense for debugging only (slow!):");
  Eigen::MatrixXi dense = Eigen::MatrixXi(visMat);
  DEBUG("visMat:");
  if (visMat.rows() >= 4 && visMat.cols() >= 4)
    Rcpp::Rcout << dense.block<4, 4>(0, 0) << std::endl;
  else
    Rcpp::Rcout << dense << std::endl;
}

#ifdef ICD_DEBUG
#define PRINTCORNERMAP(x) Rcpp::Rcout << #x << ": "; printCornerMap(x);
#define PRINTCORNERSP(x) Rcpp::Rcout << #x << ": "; printCornerSparse(x);
#define ICD_ASSIGN(row,col) mapMat(row, col) = true;
#else
#define PRINTCORNERMAP(x) ((void)0);
#define PRINTCORNERSP(x) ((void)0);
#define ICD_ASSIGN(row,col) mapMat.coeffRef(row, col) = true;
#endif

void buildVisitCodesSparseSimple(const SEXP& icd9df,
                                 const std::string& visitId,
                                 const std::string& icd9Field,
                                 const CV relevant,
                                 PtsSparse& visMat,
                                 VecStr& visitIds // get this from sparse matrix at end, but needed?
) {
  SEXP icds = PROTECT(getRListOrDfElement(icd9df, icd9Field.c_str())); // this is a factor
  SEXP vsexp = PROTECT(getRListOrDfElement(icd9df, visitId.c_str())); // this may be anything
  IntegerVector icd9dfFactor;
  switch(TYPEOF(icds)) {
  case STRSXP: {
    TRACE("got a string expression");
    icd9dfFactor = factorNoSort(icds, relevant);
    break;
  }
  case INTSXP: {
    TRACE("got integers");
    if (icd9dfFactor.attr("levels") == R_NilValue) {
      TRACE("not a factor");
      icd9dfFactor = factorNoSort(fastIntToStringRcpp(icds), relevant);
    } else {
      TRACE("is factor");
      icd9dfFactor = factorNoSort(icds, relevant);
    }
  }
  default: {
    Rcpp::stop("cannot convert this type of visit ID yet: use character or factor");
  }
  }
  CV factorLevels = icd9dfFactor.attr("levels");
  R_xlen_t numUniqueCodes = factorLevels.length();
  // random long string TODO: test consequences of going over length.
  std::string lastVisit = "JJ94967295JJ94967295JJ";
  int vlen = icd9dfFactor.size();
  // make an unordered set for quick check for duplicates while building list of unique visit ids
  VisLk vis_lookup;
  vis_lookup.reserve(vlen);
  // also maintain list of (ordered as first encountered) visit ids
  visitIds.resize(vlen); // resize and trim at end, as alternative to reserve
  int n;
  VecVecIntSz vcdb_max_idx = -1; // we increment immediately to zero as first index
  VecVecIntSz vcdb_new_idx;
  VecVecIntSz vcdb_last_idx = 2094967295; // extremely unlikely random, < 2^32 (to avoid 32bit R build warn)
  visMat.resize(vlen, numUniqueCodes); // overestimate badly to start
  visMat.reserve(vlen); // but memory commitment is known and limited.
  std::vector<Triplet> visTriplets;
  visTriplets.reserve(vlen * 30); // overestimate codes per patient to avoid resizing while filling
  // the result matrix size should have dimensions rows: number of unique
  // visitIds, cols: number of unique ICD codes.
  for (int i = 0; i != vlen; ++i) {
    TRACE("vcdb_max_idx: " << vcdb_max_idx <<
      " vcdb_new_idx: " << vcdb_new_idx <<
        " vcdb_last_idx: " <<  vcdb_last_idx);
    std::string visit = CHAR(STRING_ELT(vsexp, i)); // may not be string
    n = icd9dfFactor[i]; // ICD codes are in a factor, so get the 1-indexed integer index
    if (lastVisit != visit) {
      TRACE("visit has changed");
      vcdb_new_idx = vcdb_max_idx + 1;
      VisLk::iterator found = vis_lookup.find(visit); // did we see this visit already? Get name-index pair.
      if (found != vis_lookup.end()) { // we found the old visit
        // we saved the index in the map, so use that to insert a triplet:
        TRACE("Found " << found->first << " with row id: " << found->second);
        TRACE("adding true at index (" << found->second << ", " << n - 1 << ")");
        visTriplets.push_back(Triplet(found->second, n - 1, true));
        continue; // and continue with next row
      } else { // otherwise we found a new visitId, so add it to our lookup table
        TRACE("visit is new");
        VisLkPair vis_lookup_pair = std::make_pair(visit, vcdb_new_idx);
        vis_lookup.insert(vis_lookup_pair); // new visit, with associated position in vcdb
      }
      // we didn't find an existing visitId
      TRACE("adding true at index (" << vcdb_new_idx << ", " << n - 1 << ")");
      visTriplets.push_back(Triplet(vcdb_new_idx, n - 1, true));
      visitIds[vcdb_new_idx] = visit; // keep list of visitIds in order encountered.
      lastVisit = visit;
      vcdb_last_idx = vcdb_new_idx;
      ++vcdb_max_idx;
    } else { // last visitId was the same as the current one, so we can skip all the logic
      TRACE("adding true at index (" << vcdb_last_idx << ", " << n-1 << ")");
      visTriplets.push_back(Triplet(vcdb_last_idx, n - 1, true));
    }
  } // end loop through all visit-code input data
  UNPROTECT(2);

  // visMat and visitIds are updated
  visMat.setFromTriplets(visTriplets.begin(), visTriplets.end());
  visMat.conservativeResize(vcdb_max_idx + 1, numUniqueCodes);
  visitIds.resize(vcdb_max_idx + 1); // we over-sized (not just over-reserved) so now we trim.
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

void buildMapMat(const List& map, const CV& relevant, DenseMap& mapMat) {
  mapMat.setZero();
  std::unordered_map<int, int> map_lookup;
  std::unordered_map<int, int>::iterator found_it;
  map_lookup.reserve(relevant.length());
  R_xlen_t row = 0;
  for (List::const_iterator li = map.begin(); li != map.end(); ++li) {
    auto col = std::distance(map.begin(), li);
    TRACE("working on map item/col: " << col);
    IntegerVector v = *li;
    for (IntegerVector::iterator vi = v.begin(); vi != v.end(); ++vi) {
      TRACE("working on: " << std::distance(v.begin(), vi) << " row: " << row);
      const int this_code_factor_number = *vi;
      found_it = map_lookup.find(this_code_factor_number);
      if (found_it == map_lookup.end()) {
        TRACE("not found in lookup, adding row " << row << ", col " << col);
        ICD_ASSIGN(row, col);
        map_lookup.insert(std::make_pair(this_code_factor_number, row++));
      } else {
        TRACE("inserting dupe at row " << found_it->second << ", col " << col);
        ICD_ASSIGN(found_it->second, col); // assign in existing row/visit
      }
    }
  }
  PRINTCORNERMAP(mapMat);
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
//' @examples
//' # show how many discrete ICD codes there are in the AHRQ map, before reducing
//' # to the number which actually appear in a group of patient visitsben
//' library(magrittr)
//' sapply(icd::icd9_map_ahrq, length) %>% sum
//' @keywords internal array algebra
// [[Rcpp::export]]
LogicalMatrix comorbidMatMulMore(const DataFrame& icd9df,
                                 const List& icd9Mapping,
                                 const std::string visitId,
                                 const std::string icd9Field) {
  VecStr out_row_names; // size is reserved in buildVisitCodesVec
  const CV relevant = getRelevant(icd9Mapping, icd9df[icd9Field]);
  const List map = remap(icd9Mapping, relevant);
  DenseMap mapMat(relevant.length(), map.length());
  buildMapMat(map, relevant, mapMat);
  PtsSparse visMat; // reservation and sizing done within next function
  buildVisitCodesSparseSimple(icd9df, visitId, icd9Field, relevant,
                              visMat, out_row_names);
  if (visMat.cols() != mapMat.rows())
    Rcpp::stop("matrix multiplication won't work");
  DenseMap result = visMat * mapMat; // col major result
  DEBUG("Result rows: " << result.rows() << ", cols: " << result.cols());
  PRINTCORNERMAP(result);
  Rcpp::IntegerMatrix mat_out_int = Rcpp::wrap(result);
  Rcpp::LogicalMatrix mat_out_bool = Rcpp::wrap(mat_out_int);
  List dimnames = Rcpp::List::create(out_row_names, map.names());
  mat_out_bool.attr("dimnames") = dimnames;
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
//' @examples
//' # show how many discrete ICD codes there are in the AHRQ map, before reducing
//' # to the number which actually appear in a group of patient visitsben
//' library(magrittr)
//' sapply(icd::icd9_map_ahrq, length) %>% sum
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
{
  Rcpp::Rcout << "converting visit_codes_sparse to dense for debugging only (slow!):" << std::endl;
  Eigen::MatrixXi dense = Eigen::MatrixXi(visit_codes_sparse);
  Rcpp::Rcout << "visit_codes_sparse:" << std::endl;
  if (visit_codes_sparse.rows() >= 4 && visit_codes_sparse.cols() >= 4)
    Rcpp::Rcout << dense.block<4, 4>(0, 0) << std::endl;
  else
    Rcpp::Rcout << dense << std::endl;
}
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
// todo: boolean reduction to get flags instead of various integers
// https://eigen.tuxfamily.org/dox/group__TutorialReductionsVisitorsBroadcasting.html
//

valgrindCallgrindStop();
return mat_out_bool;
}
