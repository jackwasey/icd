#include "local.h"
#include "util.h"
#include <stdlib.h>
#include <math.h>                              // for floor
#include <stdio.h>
#include <string.h>                            // for strcmp
#include <algorithm>                           // for copy, sort, transform
#include <iterator>                            // for back_insert_iterator
#include <ostream>                             // for size_t, operator<<
#include <string>
#include <vector>

using namespace Rcpp;

// # nocov start

// [[Rcpp::export]]
List categorize_rcpp() {
  Rcerr << "Not implemented in pure C++ yet";
  return List::create();
}

// # nocov end
