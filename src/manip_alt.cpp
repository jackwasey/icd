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
#include <Rcpp.h>
#include "manip.h"
#include "is.h"
#include "convert.h"

// [[Rcpp::export]]
Rcpp::String icd9AddLeadingZeroesShortSingle(Rcpp::String x) {
  if (x == NA_STRING) {
    return (NA_STRING);
  }
  std::string s(x);
  if (!icd9IsASingleVE(x.get_cstring())) {
    switch (strlen(x.get_cstring())) {
    case 0:
      return (NA_STRING);
    case 1:
      return ("00" + s);
    case 2:
      return ("0" + s);
    }
  } else { // is V or E type
    switch (strlen(x.get_cstring())) {
    case 1:
      return (NA_STRING); // just "V" or "E"
    case 2:
      if (icd9IsASingleV(s.c_str())) {
        s.insert(1, "0");
      } else {
        s.insert(1, "00");
      }
    case 3:
      if (!icd9IsASingleV(s.c_str())) {
        s.insert(1, "0");
      }
    }
  }
  return (s);
}

// [[Rcpp::export(icd9_add_leading_zeroes_alt_cpp)]]
Rcpp::CharacterVector icd9AddLeadingZeroesDirect(Rcpp::CharacterVector x, bool short_code) {
  // a shortcut for when short codes is just to add the appropriate leading
  // zeros when the total length is <3.
  if (short_code)
    return Rcpp::sapply(x, icd9AddLeadingZeroesShortSingle);

  Rcpp::CharacterVector y = icd9DecimalToShort(x);
  return Rcpp::sapply(y, icd9AddLeadingZeroesShortSingle);
}

