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
#include "attr.h"
#include "local.h"
#include <Rcpp.h>
#include <vector>
#include <string>
#include <Rinternals.h>

//' Set class to icd10cm and icd10 in C__
//'
//' Assume either of those classes not already set
//' @param x R object
//' @examples
//' j <- "100"
//' icd:::set_class_icd10cm(j)
//' stopifnot(is.icd10(j))
//' stopifnot(is.icd10cm(j))
//' @keywords internal
// [[Rcpp::export(set_class_icd10cm)]]
void setClassIcd10cm(Rcpp::RObject& x) {
  // for now, just assume no other class already set
  Rcpp::CharacterVector cl = x.attr("class");
  cl.push_front("icd10");
  cl.push_front("icd10cm");
}
