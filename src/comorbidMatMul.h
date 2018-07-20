// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::interfaces(r, cpp)]]
#include "icd_types.h"
#include "config.h"
#include "local.h"
#include <algorithm>                   // for binary_search, copy
#include <vector>                      // for vector, vector<>::const_iterator
#include "util.h"                      // for debug_parallel
extern "C" {
#include "cutil.h"                     // for getRListOrDfElement
}

