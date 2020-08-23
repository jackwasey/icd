// [[Rcpp::depends(RcppEigen)]]
#include "comorbidMatMul.h"
#include "fastIntToString.h"
#include "icd_types.h"
#include "local.h"
#include "mapplus.h"
#include "refactor.h"
#include "relevant.h"
#include <Rcpp.h> // also includes Rinternals.h
#include <cstring>
#include <string>
#include <unordered_set>
#include <vector>

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
    Rcpp::Rcout << dense.block<4, 4>(0, 0) << std::endl;
  else
    Rcpp::Rcout << dense << std::endl;
}
#endif
// # nocov end

void buildVisitCodesSparseWide(
  const DataFrame& data,
  const std::string& id_name,
  const Rcpp::CharacterVector& code_names,
  const bool validate,
  Relevant& relevant,
  PtsSparse& visMat, // output
  VecStr& visitIds   // output: can get this from sparse matrix at end? Needed?
) {
  DEBUG_VEC(relevant.keys);
  const RObject visits = data[id_name];
  R_xlen_t vlen        = Rf_xlength(visits);
  std::vector<Triplet> visTriplets;
  const auto ncol = code_names.size();
  visTriplets.reserve(vlen * ncol); // upper bound
  IntegerVector rows = no_init(vlen * ncol);
  if (!Rf_isFactor(visits)) {
    ICD_TIME_BEGIN(notfactor);
    DEBUG(TYPEOF(visits));
    CV v     = (CV)visits; // assume character for now
    CV uv    = unique(v);
    visitIds = as<VecStr>(uv);
    rows     = match(v, uv);
    ICD_TIME_END(notfactor);
  } else {
    ICD_TIME_BEGIN(factoralready);
    rows     = visits; // can do this without copy using unique_ptr?
    visitIds = as<VecStr>(rows.attr("levels"));
    ICD_TIME_END(factoralready);
  }
  for (int j = 0; j != code_names.size(); ++j) {
    String data_col_name = code_names[j];
    const SEXP& data_col = data[data_col_name];
    if (Rf_isFactor(data_col)) {
      const IntegerVector& data_col_fc = (IntegerVector)data_col;
      DEBUG("codes are still in a factor...");
#ifdef ICD_DEAD
      const CV& code_levels = data_col_fc.attr("levels");
#endif
      const IntegerVector codes_relevant =
        refactor(data_col_fc, relevant.str_codes, true, validate);
      assert(rows.size() == codes_relevant.size());
      for (R_xlen_t i = 0; i != rows.size(); ++i) {
        DEBUG("add triplet at R idx:" << rows[i] << ", " << codes_relevant[i]);
        if (IntegerVector::is_na(codes_relevant[i])) continue;
        visTriplets.push_back(Triplet(rows[i] - 1, codes_relevant[i] - 1, true));
      } // end i loop through rows
    } else {
      const CV& data_col_cv = (CV)data_col;
      DEBUG_VEC(data_col_cv);
      for (R_xlen_t i = 0; i != rows.size(); ++i) {
        const auto found = relevant.rel_map.find(((String)data_col_cv[i]));
        if (found == relevant.rel_map.cend()) continue;
        DEBUG("adding triplet at R idx:" << rows[i] << ", " << found->second);
        visTriplets.push_back(Triplet(rows[i] - 1, found->second, true));
      } // end i loop through rows
    }   // factor vs character for this code column
  }     // end j loop through data columns
  visMat.resize(visitIds.size(), relevant.str_codes.size()); // unique ids
  visMat.reserve(vlen * ncol);                               // upper bound
  visMat.setFromTriplets(visTriplets.begin(), visTriplets.end());
  visMat.conservativeResize(visitIds.size(), relevant.str_codes.size());
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
//' @noRd
// [[Rcpp::export(comorbid_mat_mul_wide_rcpp)]]
LogicalMatrix comorbidMatMulWide(const DataFrame& data,
                                 const List& map,
                                 const std::string id_name,
                                 const CV& code_names,
                                 const bool validate) {
  VecStr out_row_names; // size is reserved in buildVisitCodesVec
  ICD_TIME_BEGIN(comorbidMatMul1);
  RObject visits = data[id_name]; // does this copy??? RObject instead?
  ICD_TIME_END(comorbidMatMul1);

  ICD_TIME_BEGIN(comorbidMatMulRel);
  Relevant r(map, data, code_names);
  ICD_TIME_END(comorbidMatMulRel);

  ICD_TIME_BEGIN(comorbidMatMulMP);
  MapPlus m(map, r);
  ICD_TIME_END(comorbidMatMulMP);

  PtsSparse visMat; // reservation and sizing done within next function
  ICD_TIME_BEGIN(comorbidMatMulToBuildSparse);
  buildVisitCodesSparseWide(data, id_name, code_names, validate, r, visMat, out_row_names);
  ICD_TIME_END(comorbidMatMulToBuildSparse);
  if (visMat.cols() != m.rows()) stop("matrix multiplication won't work");
  // consider using R's C interface for creating logical SEXP, then use
  // Eigen::Map to use that memory to avoid a potentially big copy. Not
  // bottleneck. The bottleneck is probably R garbage collecting.
  /*
   * Something like:
   *
   * SEXP result_sexp = PROTECT(allocVector(LGLXSP, visMat.rows() * m.cols()));
   * result_ptr = LOGICAL(result_sexp);
   *   const auto map_rows = visMat.rows();
   *   const auto map_cols = m.cols();
   *   Eigen::Map<Eigen::Matrix<int, Eigen::Dynamic, Eigen::Dynamic> >
   * result_mem_mapped;
   */
  ICD_TIME_BEGIN(matmul);
  Eigen::MatrixXi result = visMat * m.mat; // col major result
  ICD_TIME_END(matmul);
  DEBUG("Result rows: " << result.rows() << ", cols: " << result.cols());
  PRINTCORNERMAP(result);
  // I think the Rcpp Eigen code calls eval() if needed. Do I need to? If it
  // fails without, then yes, as data member would not have been available.
  // result.eval()
  //
  // See
  // https://cran.r-project.org/doc/manuals/R-exts.html#Vector-accessor-functions
  // for possibly faster way to access R vector data more quickly.
  //

  // all these timings are very small compared to the setup time
  // .eval() does start to take longer than wrap doing it, with big n
  ICD_TIME_BEGIN(eval);
  auto resok = result.eval();
  ICD_TIME_END(eval);
  // The following works: but additive time: `LogicalMatrix mat_out_bool = wrap(result);`
  // not much slower than eval. big memcopy involved.
  //
  // Better still, I can create an SEXP of the correct size, and (hopefully)
  // bulk memcopy the data into it directly. I do not think there is a way to
  // share the memory with R. Possibly set up the SEXP memory, then perhaps I
  // can tell Eigen to use it?
  ICD_TIME_BEGIN(wrap_to_int);
  // not much slower than eval. big memcopy - does it
  // incidentally do any casting? Eigen probably faster as
  // it can combine sub-operations with the mat mul.
  IntegerMatrix mat_out_int = wrap(result);
  ICD_TIME_END(wrap_to_int);
  // LogicalMatrix mat_out_bool    = wrap(mat_out_int); // slowest of all
  // INTSXP is same data structure in R as LGLSXP, so need to do anything... but
  // Rcpp thinks so. Rcpp does this to an integer vector in r_coerce.h: int
  // r_coerce<INTSXP,LGLSXP>(int from){ return ( from == NA_INTEGER ) ?
  // NA_LOGICAL : (from!=0)
  ICD_TIME_BEGIN(to_bool);
  LogicalMatrix mat_out_bool = (LogicalMatrix)mat_out_int;
  ICD_TIME_END(to_bool);

  List dimnames                 = List::create(out_row_names, map.names());
  mat_out_bool.attr("dimnames") = dimnames;
  return mat_out_bool;
}
