// [[Rcpp::depends(RcppEigen)]]
#include "icd_types.h"
#include "local.h"
#include <algorithm>                   // for binary_search, copy
#include <vector>                      // for vector, vector<>::const_iterator
extern "C" {
#include "cutil.h"                     // for getRListOrDfElement
}

