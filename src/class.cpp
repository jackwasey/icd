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

// [[Rcpp::export]]
void setDecimalCodeInPlace(Rcpp::RObject& x) {
  x.attr("icd_short_code") = false;
}

// [[Rcpp::export]]
Rcpp::RObject setDecimalCode(Rcpp::RObject& x) {
  x.attr("icd_short_code") = false;
  return x;
}

// [[Rcpp::export(icd_set_short_code)]]
void setShortCode(Rcpp::RObject& x) {
  x.attr("icd_short_code") = true;
}

/*
// [[Rcpp::export]]
SEXP icd9Cpp(SEXP x) {
  SEXP cl;

 // I may need to use SEXP for input, but I think Rcpp is going to make this
 // much easier so I can simply adda  string to a character vector...

  // need to get class vector (read only), duplicate it with size + 1 (if
  // necessary) so I can PROTECT.
  PROTECT(cl = Rf_allocVector(STRSXP, 1));
  Rf_classgets(x, cl);

  bool found = FALSE;
  // short class list, so linear search:
  for (R_xlen_t i =0; i < Rf_xlength(cl); ++i) {
    if(strcmp(cl[i], Rf_mkChar("icd9")) == 0) {
  }

 // const char* attr_s = CHAR(asChar(attr));
  UNPROTECT(1);
  return x;
}
*/
