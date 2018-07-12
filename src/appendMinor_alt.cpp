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

#include "icd_types.h"                       // for VecStr, CV, Str
#include "appendMinor.h"
#include <string.h>                          // for strlen
#include <algorithm>                         // for fill
#include <iterator>                          // for distance
#include <string>                            // for basic_string, operator!=
#include <vector>                            // for vector, vector<>::iterator
#include "is.h"                              // for icd9IsASingleVE

// Same as icd9MajMinToCode but assume codes are already trimmed and correctly
// padded with zeros, e.g. E001, V09, 001. This version does handle NA values
// correctly.
// [[Rcpp::export]]
CV icd9MajMinToCode_alt_PrePadded(const CV mjr,
                                  const CV mnr,
                                  bool isShort) {
  R_xlen_t sz = mjr.size();
  CV r_out(sz);
  Rcpp::String mnrelem;
  Rcpp::String outelem;
  for (R_xlen_t i = 0; i != sz; ++i) {
    mnrelem = (Rcpp::String)mnr[i];
    outelem = (Rcpp::String)mjr[i];
    if (mnrelem == NA_STRING) {
      mnrelem = "";
    }
    if (!isShort && mnrelem != "") {
      outelem.push_back(".");
    }
    outelem.push_back(mnrelem);
    r_out[i] = outelem;
  }
  return r_out;
}

// [[Rcpp::export]]
VecStr icd9MajMinToCode_alt_Std(const VecStr& mjr, const VecStr& mnr, bool isShort) {
  VecStr::size_type mjsz = mjr.size();
  VecStr out(mjsz);
  VecStr::size_type j;
  for (j = 0; j != mjsz; ++j) {
    out[j] = mjr[j];
    if (!isShort && mnr[j] != "") {
      out[j].append(".");
    }
    out[j].append(mnr[j]);
  }
  return out;
}

// [[Rcpp::export]]
VecStr icd9MajMinToShort_alt_Std(const VecStr& mjr, const VecStr& mnr) {
  if (mjr.size() != 1)
    return icd9MajMinToCode_alt_Std(mjr, mnr, true);
  Str m;
  m.reserve(5);
  m = mjr[0];
  VecStr newmjr(mnr.size(), m);
  icd9AppendMinorsShort(newmjr, mnr);
  return newmjr;
}

// [[Rcpp::export]]
VecStr icd9MajMinToShortSingle_alt_Std(const Str& mjr, const VecStr& mnr) {
  VecStr newmjr(mnr.size(), mjr);
  return icd9MajMinToShort_alt_Std(newmjr, mnr);
}
