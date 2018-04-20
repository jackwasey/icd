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

// [[Rcpp::interfaces(r, cpp)]]
//#include <Rcpp.h>                       // for wrap
#include "attr.h"
#include <Rcpp/r/headers.h>             // for Rf_install
#include "Rcpp.h"                       // for wrap
#include "Rcpp/api/meat/proxy.h"        // for AttributeProxyPolicy::Attribu...
#include "Rcpp/proxy/AttributeProxy.h"  // for AttributeProxyPolicy<>::Attri...

//' Set ICD short-form diagnosis code attribute
//'
//' Doing this in an R function doesn't work for 'void' equivalent, and does a
//' copy if the updated object is returned.
//' @examples
//' j <- "100"
//' attr(j, "icd_short_diag") <- TRUE
//' j
//' attr(j, "icd_short_diag") <- FALSE
//' j
//' icd:::attr_decimal_diag(j)
//' as.decimal_diag(j)
//' # if pryr is installed, use address and refs to see what is going on
//' @keywords internal attribute
// [[Rcpp::export(attr_decimal_diag)]]
void setDecimalDiag(Rcpp::RObject& x, bool value = true) {
  x.attr("icd_short_diag") = !value;
}

//' Set short diagnosis flag in C++
//' @param x Any R object
//' @param value \code{TRUE} or \code{FALSE}
//' @keywords internal attribute
// [[Rcpp::export(attr_short_diag)]]
void setShortDiag(Rcpp::RObject& x, bool value = true) {
  x.attr("icd_short_diag") = value;
}
// modification in-place is supposedly a bug (email from Tomas Kalibera)
//
// //' @rdname as.short_diag
// //' @keywords attribute
// //' @export
// // [[Rcpp::export(as.short_diag)]]
// Rcpp::RObject asShortDiag(Rcpp::RObject& x, bool value = true) {
//   x.attr("icd_short_diag") = value;
//   return x;
// }
//
// //' @rdname as.short_diag
// //' @keywords attribute
// //' @export
// // [[Rcpp::export(as.decimal_diag)]]
// Rcpp::RObject asDecimalDiag(Rcpp::RObject& x, bool value = true) {
//   x.attr("icd_short_diag") = !value;
//   return x;
// }
