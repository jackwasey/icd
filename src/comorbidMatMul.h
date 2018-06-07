// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::interfaces(r, cpp)]]
#include "config.h"                     // for valgrind, CXX11 etc
#include "local.h"                     // for ICD_OPENMP
#include "comorbidCommon.h"
#include "comorbidSetup.h"
#include <algorithm>                   // for binary_search, copy
#include <vector>                      // for vector, vector<>::const_iterator
#include "icd_types.h"                 // for ComorbidOut, VecVecInt, VecVec...
#include "util.h"                     // for debug_parallel
extern "C" {
#include "cutil.h"                              // for getRListOrDfElement
}


/*
 *
 *  Add header guard if this is reactivated.
 */

/*
using std::string;

void buildVisitCodesSparseSimple(const SEXP& icd9df,
                                 const string& visitId,
                                 const string& icd9Field,
                                 const CV relevant,
                                 PtsSparse& visMat,
                                 VecStr& visitIds);

void buildVisitCodesSparse(const SEXP& icd9df,
                           const string& visitId,
                           const string& icd9Field,
                           PtsSparse& sparse_db,
                           VecStr& visitIds);

Rcpp::LogicalMatrix comorbidMatMulSimple(const Rcpp::DataFrame& icd9df,
                                 const Rcpp::List& map,
                                 const string visitId,
                                 const string icd9Field);

Rcpp::LogicalMatrix comorbidMatMul(const Rcpp::DataFrame& icd9df,
                             const Rcpp::List& map,
                             const string visitId,
                             const string icd9Field,
                             const int threads = 8, const int chunk_size = 256,
                             const int omp_chunk_size = 1);
*/
