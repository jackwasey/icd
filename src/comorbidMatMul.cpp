// [[Rcpp::depends(RcppEigen)]]
#include "icd_types.h"
#include "local.h"
#include "valgrind_icd.h"
#include "fastIntToString.h"
#include <algorithm>                   // for binary_search, copy
#include <vector>                      // for vector, vector<>::const_iterator
#include <unordered_set>
#include "refactor.h"
#include <string>
#include <cstring>
#include "comorbidMatMul.h"
#include "relevant.h"
#include "mapplus.h"

/*
 MAKEFLAGS=-j16 R -e 'devtools::load_all(); icd9_comorbid_ahrq(ahrq_test_dat)'
 MAKEFLAGS=-j16 R -e 'devtools::load_all(); test(reporter="Location")'
 MAKEFLAGS=-j16 R -e 'devtools::load_all(); mydf <- data.frame(visit_id = c("a", "a"), "dx1" = c("441", "41293"), "dx2" = c(NA, "1001"), stringsAsFactors = TRUE); comorbidMatMulWide(mydf, icd9_map_charlson, "visit_id", c("dx1", "dx2"))'
 */

using namespace Rcpp;

// # nocov start
#ifdef ICD_DEBUG
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
#endif
// # nocov end

// icd codes are either factor or character type. if a factor, we can use the
// factor level index as the column in the visit matrix, but we need to know
// this also to construct the comorbidity matrix where the codes are rows.
void buildVisitCodesSparseSimple(
    const RObject& visits,
    const RObject& codes, // todo handle factor in parent function
    Relevant& rh,
    PtsSparse& visMat, // output
    VecStr& visitIds // output: can get this from sparse matrix at end? Needed?
) {
  TRACE("Starting build of visit matrix");
  assert(Rf_length(visits) == Rf_length(codes));
  R_xlen_t vlen = Rf_length(visits);
  DEBUG("relevant size = " << rh.relevant.size() << ", vlen = " << vlen);
  std::vector<Triplet> visTriplets;
  visTriplets.reserve(vlen);
  IntegerVector rows, cols;
  if (!Rf_isFactor(codes)) {
    DEBUG("codes are still character...");
    const CV& data_col_cv = (CV) codes;
    DEBUG_VEC(data_col_cv);
    DEBUG_VEC(rh.keys);
    DEBUG("rh.keys.size() = " << rh.keys.size());
    cols = rh.hash.lookup(data_col_cv); // C indexed
  } else {
    DEBUG("codes are still in a factor...");
    CV code_levels = wrap(codes.attr("levels"));
    const IntegerVector codes_relevant =
      refactor((IntegerVector) codes, rh.relevant, true); // no NA in output levels, please
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
    rows = match(v, uv);
    // matrix row names are character. TODO: consider option not to
    // name the rows, e.g. for people who just summarize the data: will save a
    // lot of character processing
  } else {
    DEBUG("visits are factors");
    rows = visits; // can do this without copy using unique_ptr?
    visitIds = as<VecStr>(rows.attr("levels"));
  }
  DEBUG_VEC(rows);
  DEBUG_VEC(cols);
  DEBUG("n rows: " << rows.size() << ", n cols: " << cols.size());
  assert(rows.size() == cols.size());
  visMat.resize(visitIds.size(), rh.relevant.size()); // unique ids
  visMat.reserve(vlen); // number of triplets is just vlen (for long data)

  // now we have rows and columns, just make the triplets and insert.
  for (R_xlen_t i = 0; i != rows.size(); ++i) {
    if (IntegerVector::is_na(cols[i])) continue;
    TRACE("adding triplet at R idx:" << rows[i] << ", " << cols[i]);
    visTriplets.push_back(Triplet(rows[i] - 1, cols[i] - 1, true));
  }
  TRACE("visMat rows: " << visMat.rows() << ", cols: " << visMat.cols());
  TRACE("visMat to be updated, setting visMat from triplets...");
  TRACE("there are " << visTriplets.size() << " triplets");
  visMat.setFromTriplets(visTriplets.begin(), visTriplets.end());
  visMat.conservativeResize(visitIds.size(), rh.relevant.size());
  DEBUG("Built visMt, rows:" << visMat.rows() << ", cols: " << visMat.cols());
  PRINTCORNERSP(visMat);
}

void buildVisitCodesSparseWide(
    const DataFrame& data,
    const std::string id_name,
    const CV code_names,
    Relevant& rh,
    PtsSparse& visMat, // output
    VecStr& visitIds // output: can get this from sparse matrix at end? Needed?
) {
  DEBUG_VEC(rh.keys);
  const RObject visits = data[id_name];
  R_xlen_t vlen = Rf_length(visits);
  std::vector<Triplet> visTriplets;
  auto ncol = code_names.size();
  visTriplets.reserve(vlen * ncol); // upper bound
  IntegerVector rows = Rcpp::no_init(vlen * ncol);
  if (!Rf_isFactor(visits)) {
    DEBUG(TYPEOF(visits));
    CV v = (CV) visits; // assume character for now
    CV uv = unique(v);
    visitIds = as<VecStr>(uv);
    rows = match(v, uv);
  } else {
    rows = visits; // can do this without copy using unique_ptr?
    visitIds = as<VecStr>(rows.attr("levels"));
  }
  for (int j = 0; j != code_names.size(); ++j) {
    String data_col_name = code_names[j];
    const SEXP& data_col = data[data_col_name];
    if (Rf_isFactor(data_col)) {
      const IntegerVector& data_col_fc = (IntegerVector) data_col;
      DEBUG("codes are still in a factor...");
      const CV code_levels = data_col_fc.attr("levels");
      const IntegerVector codes_relevant =
        refactor(data_col_fc, rh.relevant, true); // no NA in output levels, please
      assert(rows.size() == codes_relevant.size());
      for (R_xlen_t i = 0; i != rows.size(); ++i) {
        DEBUG("adding triplet at R idx:" << rows[i] << ", " << codes_relevant[i]);
        if (IntegerVector::is_na(codes_relevant[i])) continue;
        visTriplets.push_back(Triplet(rows[i] - 1, codes_relevant[i] - 1, true));
      } // end i loop through rows
    } else {
      const CV& data_col_cv = (CV) data_col;
      DEBUG_VEC(data_col_cv);
      for (R_xlen_t i = 0; i != rows.size(); ++i) {
        auto found = rh.rel.find(((String) data_col_cv[i]).get_cstring());
        if (found == rh.rel.cend()) continue;
        DEBUG("adding triplet at R idx:" << rows[i] << ", " << found->second);
        visTriplets.push_back(Triplet(rows[i] - 1, found->second, true));
      } // end i loop through rows
    } // factor vs character for this code column
  } // end j loop through data columns
  visMat.resize(visitIds.size(), rh.relevant.size()); // unique ids
  visMat.reserve(vlen * ncol); // upper bound
  visMat.setFromTriplets(visTriplets.begin(), visTriplets.end());
  visMat.conservativeResize(visitIds.size(), rh.relevant.size());
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
LogicalMatrix comorbidMatMulWide(const DataFrame& data,
                                 const List& map,
                                 const std::string id_name,
                                 const CV code_names) {
  VecStr out_row_names; // size is reserved in buildVisitCodesVec
  RObject visits = data[id_name]; // does this copy??? RObject instead?

  //TODO: Relevant requires CV right now, not factor
  // Does making a data.frame with subset of columns make a deep copy?
  Relevant r(map, data, code_names);
  MapPlus m(map, r);
  PtsSparse visMat; // reservation and sizing done within next function
  buildVisitCodesSparseWide(data, id_name, code_names,
                            r, visMat, out_row_names);
  if (visMat.cols() != m.rows())
    Rcpp::stop("matrix multiplication won't work");
  DenseMap result = visMat * m.mat; // col major result
  DEBUG("Result rows: " << result.rows() << ", cols: " << result.cols());
  PRINTCORNERMAP(result);
  Rcpp::IntegerMatrix mat_out_int = Rcpp::wrap(result);
  Rcpp::LogicalMatrix mat_out_bool = Rcpp::wrap(mat_out_int);
  List dimnames = Rcpp::List::create(out_row_names, map.names());
  mat_out_bool.attr("dimnames") = dimnames;
  return mat_out_bool;
}

//' @rdname comorbidMatMulWide
//' @keywords internal array algebra
// [[Rcpp::export]]
LogicalMatrix comorbidMatMulSimple(const DataFrame& data,
                                   const List& map,
                                   const std::string id_name,
                                   const std::string code_name) {
  VecStr out_row_names; // size is reserved in buildVisitCodesVec
  RObject visits = data[id_name]; // does this copy??? RObject instead?
  CV codes = data[code_name];
  //TODO: Relevant requires CV right now, not factor
  Relevant r(map, codes); // potential to template over codes type
  MapPlus m(map, r);
  PtsSparse visMat; // reservation and sizing done within next function
  buildVisitCodesSparseSimple(visits, codes, r, visMat, out_row_names);
  if (visMat.cols() != m.rows())
    Rcpp::stop("matrix multiplication won't work");
  DenseMap result = visMat * m.mat; // col major result
  DEBUG("Result rows: " << result.rows() << ", cols: " << result.cols());
  PRINTCORNERMAP(result);
  Rcpp::IntegerMatrix mat_out_int = Rcpp::wrap(result);
  Rcpp::LogicalMatrix mat_out_bool = Rcpp::wrap(mat_out_int);
  List dimnames = Rcpp::List::create(out_row_names, map.names());
  mat_out_bool.attr("dimnames") = dimnames;
  return mat_out_bool;
}
