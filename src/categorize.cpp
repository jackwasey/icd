// Copyright (C) 2014 - 2018  Jack O. Wasey
//
// This file is part of icd.
//
// icd is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// icd is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with icd. If not, see <http://www.gnu.org/licenses/>.

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
#ifdef ICD_OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

// [[Rcpp::export]]
List categorize_rcpp() {
  Rcpp::Rcerr << "Not implemented in pure C++ yet";
  return List::create();
}
