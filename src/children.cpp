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

#include "local.h"
#include <Rcpp.h>
#include <vector>
#include <string>
// #include <Rinternals.h>

//
//  # matches, nc, matches, last_row
//  for (i in seq_along(icd10Short)) {
//    # now the children, assuming the source file is sorted logically, will be
//    # subsequent codes, until a code of the same length is found
//    check_row <- matches[i] + 1
//    parent_len <- nc[matches[i]]
//    while (nc[check_row] > parent_len && check_row != last_row + 1)
//      check_row <- check_row + 1
//
//    kids <- c(kids, icd::icd10cm2016[matches[i]:(check_row - 1), "code"])
//  }
//
//  as.icd10cm(kids, short_code)

// [[Rcpp::export(icd10cm_children_defined_cpp)]]
Rcpp::CharacterVector icd10cmChildrenDefined(Rcpp::CharacterVector &x) {

  // need namespace for sysdata (.nc) and package env for lazy data
  Rcpp::Environment env("package:icd"); // only works when package is loaded
  Rcpp::Environment ns = Rcpp::Environment::namespace_env("icd");
  Rcpp::List icd10cm2016 = env["icd10cm2016"];
  Rcpp::CharacterVector allCodes = icd10cm2016["code"];
  Rcpp::IntegerVector nc = ns[".nc"];
  Rcpp::IntegerVector matchesNa = Rcpp::match(x, allCodes);
  // now drop NA rows, Rcpp match doesn't do this, yet.
  // match(x, table, nomatch = 0L) > 0L
  Rcpp::IntegerVector matches = matchesNa[!is_na(matchesNa)]; // 1-based index (R style)

  std::vector<std::string> kids;

  if (matches.length() == 0) {
    if (x.length() > 0) {
      Rcpp::warning("None of the provided ICD-10 codes matched the master ICD-10-CM list (currently 2016)");
    }
    return(Rcpp::CharacterVector(0));
  }

  kids.reserve(x.length() * 10);

  Rcpp::CharacterVector tmp = icd10cm2016[0];
  int last_row = tmp.length(); // zero-based index
  int check_row; // zero-based index
  int parent_len; // number of characters in original parent code

  for (int i = 0; i != matches.length(); ++i) {
    check_row = matches[i] + 1 - 1; // check the row after the parent (may be off end of vector)
    parent_len = nc[matches[i] - 1];
    while (check_row < last_row && nc[check_row] > parent_len)
      ++check_row;

    Rcpp::CharacterVector::iterator it = allCodes.begin();
    std::advance(it, matches[i] - 1);
    Rcpp::CharacterVector::iterator it2 = allCodes.begin();
    std::advance(it2, check_row);
    kids.insert(kids.end(), it, it2);
  }
  return Rcpp::wrap(kids);
}
