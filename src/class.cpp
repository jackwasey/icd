// Copyright (C) 2014 - 2016  Jack O. Wasey
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

// [[Rcpp::interfaces(r, cpp)]]
#include "class.h"
#include "local.h"
#include <Rcpp.h>
#include <vector>
#include <string>
#include <Rinternals.h>

//' Set icd_short_diag attribute in-place
//'
//' Doing this in an R function doesn't work for 'void' equivalent, and does a
//' copy if the updated object is returned.
//' @examples
//' j <- "100"
//' attr(j, "icd_short_diag") <- TRUE
//' j
//' attr(j, "icd_short_diag") <- FALSE
//' j
//' icd:::.attr_decimal_diag(j)
//' as_decimal_diag(j)
//' \dontrun{
//' library(pryr)
//' j <- 1
//' c(address(j), refs(j))
//' attr(j, "icd_short_diag") <- TRUE
//' c(address(j), refs(j))
//' attr(j, "icd_short_diag") <- FALSE
//' c(address(j), refs(j))
//' icd:::.attr_decimal_diag(j)
//' c(address(j), refs(j))
//' j <- as_decimal_diag(j)
//' c(address(j), refs(j))
//' # Rcpp cleverer than R, and doesn't return a copy of the data
//' }
//' @keywords internal attribute
//' @rdname as.icd_short_diag
// [[Rcpp::export(.attr_decimal_diag)]]
void setDecimalDiag(Rcpp::RObject& x, bool value) {
  x.attr("icd_short_diag") = !value;
}

//' @rdname as.icd_short_diag
//' @keywords internal attribute
// [[Rcpp::export(.attr_short_diag)]]
void setShortDiag(Rcpp::RObject& x, bool value) {
  x.attr("icd_short_diag") = value;
}

//' @rdname as.icd_short_diag
//' @keywords attribute
//' @export
// [[Rcpp::export(as.icd_short_diag)]]
Rcpp::RObject asShortDiag(Rcpp::RObject& x, bool value = true) {
  x.attr("icd_short_diag") = value;
  return x;
}

//' @rdname as.icd_short_diag
//' @keywords attribute
//' @export
// [[Rcpp::export(as.icd_decimal_diag)]]
Rcpp::RObject asDecimalDiag(Rcpp::RObject& x, bool value = true) {
  x.attr("icd_short_diag") = !value;
  return x;
}
