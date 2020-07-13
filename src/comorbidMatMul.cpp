// [[Rcpp::depends(RcppEigen)]]
#include "comorbidMatMul.h"
#include "fastIntToString.h"
#include "icd_types.h"
#include "local.h"
#include "mapplus.h"
#include "refactor.h"
#include "relevant.h"
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
  const CV &code_names,
  const bool validate,
  Relevant &relevant,
  PtsSparse &visMat, // output
  VecStr &visitIds   // output: can get this from sparse matrix at end? Needed?
) {
  DEBUG_VEC(relevant.keys);
  const RObject visits = data[id_name];
  R_xlen_t vlen        = Rf_length(visits);
  std::vector<Triplet> visTriplets;
  const auto ncol = code_names.size();
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
      const CV &code_levels = data_col_fc.attr("levels");
      const IntegerVector codes_relevant =
        refactor(data_col_fc, relevant.str_codes, true, validate);
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
        const auto found = relevant.rel_map.find(((String)data_col_cv[i]).get_cstring());
        if (found == relevant.rel_map.cend()) continue;
        DEBUG("adding triplet at R idx:" << rows[i] << ", " << found->second);
        visTriplets.push_back(Triplet(rows[i] - 1, found->second, true));
      } // end i loop through rows
    }   // factor vs character for this code column
  }     // end j loop through data columns
  visMat.resize(visitIds.size(), relevant.str_codes.size()); // unique ids
  visMat.reserve(vlen * ncol);                        // upper bound
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
LogicalMatrix comorbidMatMulWide(const DataFrame &data,
                                 const List &map,
                                 const std::string id_name,
                                 const CV &code_names,
                                 const bool validate) {
  VecStr out_row_names;           // size is reserved in buildVisitCodesVec
  RObject visits = data[id_name]; // does this copy??? RObject instead?
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
  // consider using R's C interface for creating logical SEXP, then use Eigen::Map to use that memory to avoid a potentially big copy.
  /*
   * Something like:
   *
   * SEXP result_sexp = PROTECT(allocVector(LGLXSP, visMat.rows() * m.cols()));
   * result_ptr = LOGICAL(result_sexp);
   *   const auto map_rows = visMat.rows();
   *   const auto map_cols = m.cols();
   *   Eigen::Map<Eigen::Matrix<int, Eigen::Dynamic, Eigen::Dynamic> > result_mem_mapped;
  */
  Eigen::MatrixXi result = visMat * m.mat; // col major result
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

#ifdef ICD_TIME
  // all these timings are very small compared to the setup time
  high_resolution_clock::time_point t0 = high_resolution_clock::now();
#endif // ICD_TIME
  auto resok = result.eval(); // does start to take longer than wrap with big n
#ifdef ICD_TIME
  high_resolution_clock::time_point t1 = high_resolution_clock::now();
#endif // ICD_TIME
  // works: but additive time: LogicalMatrix mat_out_bool     = wrap(result); //
  // not much slower than eval. big memcopy
  //
  // Better still, I can create an SEXP of the correct size, and (hopefully)
  // bulk memcopy the data into it directly. I do not think there is a way to
  // share the memory with R. Possibly set up the SEXP memory, then perhaps I
  // can tell Eigen to use it?
  IntegerMatrix mat_out_int     = wrap(result); // not much slower than eval. big memcopy - does it incidentally do any casting?
#ifdef ICD_TIME
  high_resolution_clock::time_point t2 = high_resolution_clock::now();
#endif // ICD_TIME
  //LogicalMatrix mat_out_bool    = wrap(mat_out_int); // slowest of all
  // INTSXP is same data structure in R as LGLSXP, so need to do anything... but Rcpp thinks so.
  // Rcpp does this to an integer vector in r_coerce.h: int r_coerce<INTSXP,LGLSXP>(int from){ return ( from == NA_INTEGER ) ? NA_LOGICAL : (from!=0)
  LogicalMatrix mat_out_bool = (LogicalMatrix)mat_out_int;
#ifdef ICD_TIME
  high_resolution_clock::time_point t3 = high_resolution_clock::now();
#endif // ICD_TIME

#ifdef ICD_TIME
  Rcout << "eval: " << duration_cast<duration<double>>(t1 - t0).count() << " seconds.";
  Rcout << std::endl;
  Rcout << "wrap: " << duration_cast<duration<double>>(t2 - t1).count() << " seconds.";
  Rcout << std::endl;
  Rcout << "bool: " << duration_cast<duration<double>>(t3 - t2).count() << " seconds.";
  Rcout << std::endl;
#endif // ICD_TIME
  List dimnames                 = List::create(out_row_names, map.names());
  mat_out_bool.attr("dimnames") = dimnames;
  return mat_out_bool;
}
