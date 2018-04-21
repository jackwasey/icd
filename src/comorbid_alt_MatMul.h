// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::interfaces(r, cpp)]]
#include "config.h"                     // for valgrind, CXX11 etc
#include "local.h"                     // for ICD_OPENMP
#ifdef ICD_EIGEN // rest of file
#include "comorbidCommon.h"
#include "comorbidSetup.h"
#include <algorithm>                   // for binary_search, copy
#include <vector>                      // for vector, vector<>::const_iterator
#include "icd_types.h"                 // for ComorbidOut, VecVecInt, VecVec...
#include "util.h"                     // for debug_parallel
extern "C" {
#include "cutil.h"                              // for getRListOrDfElement
}

using namespace Rcpp;

void buildVisitCodesVecSparse(const SEXP& icd9df,
                              const std::string& visitId,
                              const std::string& icd9Field,
                              PtsSparse& sparse_db,
                              VecStr& visitIds);

LogicalMatrix icd9Comorbid_alt_MatMul(const Rcpp::DataFrame& icd9df, const Rcpp::List& icd9Mapping,
                                      const std::string visitId, const std::string icd9Field,
                                      const int threads = 8, const int chunk_size = 256,
                                      const int omp_chunk_size = 1);

#endif // ICD_EIGEN
