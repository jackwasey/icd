#include "local.h"
#include "util.h"
#include <algorithm> // for copy, sort, transform
#include <iterator>  // for back_insert_iterator
#include <math.h>    // for floor
#include <ostream>   // for size_t, operator<<
#include <stdio.h>
#include <stdlib.h>
#include <string.h> // for strcmp
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
