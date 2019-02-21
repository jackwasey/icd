// [[Rcpp::depends(RcppEigen)]]
#include "comorbidMatMul.h"
#include "fastIntToString.h"
#include "icd_types.h"
#include "local.h"
#include "mapplus.h"
#include "refactor.h"
#include "relevant.h"
#include "valgrind_icd.h"
#include <cstring>
#include <string>
#include <unordered_set>
#include <vector>

/*
 MAKEFLAGS=-j16 R -e 'devtools::load_all(); icd9_comorbid_ahrq(ahrq_test_dat)'
 MAKEFLAGS=-j16 R -e 'devtools::load_all(); test(reporter="Location")'
 MAKEFLAGS=-j16 R -e 'devtools::load_all(); mydf <- data.frame(visit_id = c("a",
 "a"), "dx1" = c("441", "41293"), "dx2" = c(NA, "1001"), stringsAsFactors =
 TRUE); comorbidMatMulWide(mydf, icd9_map_charlson, "visit_id", c("dx1",
 "dx2"))'
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
    DEBUG(x(0, 0) << ", " << x(0, 1) << ", " << x(1, 0) << ", " << x(1, 1));
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
    Rcout << dense.block<4, 4>(0, 0) << std::endl;
  else
    Rcout << dense << std::endl;
}
#endif
// # nocov end

void buildVisitCodesSparseWide(
  const DataFrame &data,
  const std::string id_name,
  const CV code_names,
  const bool validate,
  Relevant &rh,
  PtsSparse &visMat, // output
  VecStr &visitIds   // output: can get this from sparse matrix at end? Needed?
) {
  DEBUG_VEC(rh.keys);
  const RObject visits = data[id_name];
  R_xlen_t vlen        = Rf_length(visits);
  std::vector<Triplet> visTriplets;
  auto ncol = code_names.size();
  visTriplets.reserve(vlen * ncol); // upper bound
  IntegerVector rows = no_init(vlen * ncol);
  if (!Rf_isFactor(visits)) {
    DEBUG(TYPEOF(visits));
    CV v     = (CV)visits; // assume character for now
    CV uv    = unique(v);
    visitIds = as<VecStr>(uv);
    rows     = match(v, uv);
  } else {
    rows     = visits; // can do this without copy using unique_ptr?
    visitIds = as<VecStr>(rows.attr("levels"));
  }
  for (int j = 0; j != code_names.size(); ++j) {
    String data_col_name = code_names[j];
    const SEXP &data_col = data[data_col_name];
    if (Rf_isFactor(data_col)) {
      const IntegerVector &data_col_fc = (IntegerVector)data_col;
      DEBUG("codes are still in a factor...");
      const CV code_levels = data_col_fc.attr("levels");
      const IntegerVector codes_relevant =
        refactor(data_col_fc, rh.relevant, true, validate);
      assert(rows.size() == codes_relevant.size());
      for (R_xlen_t i = 0; i != rows.size(); ++i) {
        DEBUG("add triplet at R idx:" << rows[i] << ", " << codes_relevant[i]);
        if (IntegerVector::is_na(codes_relevant[i])) continue;
        visTriplets.push_back(
          Triplet(rows[i] - 1, codes_relevant[i] - 1, true));
      } // end i loop through rows
    } else {
      const CV &data_col_cv = (CV)data_col;
      DEBUG_VEC(data_col_cv);
      for (R_xlen_t i = 0; i != rows.size(); ++i) {
        auto found = rh.rel.find(((String)data_col_cv[i]).get_cstring());
        if (found == rh.rel.cend()) continue;
        DEBUG("adding triplet at R idx:" << rows[i] << ", " << found->second);
        visTriplets.push_back(Triplet(rows[i] - 1, found->second, true));
      } // end i loop through rows
    }   // factor vs character for this code column
  }     // end j loop through data columns
  visMat.resize(visitIds.size(), rh.relevant.size()); // unique ids
  visMat.reserve(vlen * ncol);                        // upper bound
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
LogicalMatrix comorbidMatMulWide(const DataFrame &data,
                                 const List &map,
                                 const std::string id_name,
                                 const CV code_names,
                                 const bool validate) {
  VecStr out_row_names;           // size is reserved in buildVisitCodesVec
  RObject visits = data[id_name]; // does this copy??? RObject instead?

  // TODO: Relevant requires CV right now, not factor
  // Does making a data.frame with subset of columns make a deep copy?
  Relevant r(map, data, code_names);
  MapPlus m(map, r);
  PtsSparse visMat; // reservation and sizing done within next function
  buildVisitCodesSparseWide(data,
                            id_name,
                            code_names,
                            validate,
                            r,
                            visMat,
                            out_row_names);
  if (visMat.cols() != m.rows()) stop("matrix multiplication won't work");
  DenseMap result = visMat * m.mat; // col major result
  DEBUG("Result rows: " << result.rows() << ", cols: " << result.cols());
  PRINTCORNERMAP(result);
  IntegerMatrix mat_out_int     = wrap(result);
  LogicalMatrix mat_out_bool    = wrap(mat_out_int);
  List dimnames                 = List::create(out_row_names, map.names());
  mat_out_bool.attr("dimnames") = dimnames;
  return mat_out_bool;
}
