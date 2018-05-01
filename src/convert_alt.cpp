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

#include "convert_alt.h"
#include <Rcpp.h>       // for NA_STRING, Rf_install
#include <string.h>               // for strlen
#include "is.h"                   // for icd9IsASingleVE, icd9IsASingleE
#include "util.h"                 // for strimCpp

// [[Rcpp::export]]
CV icd9MajMinToCode_alt_Old(CV mjr, CV mnr, bool isShort) {
#ifdef ICD_DEBUG_TRACE
  Rcpp::Rcout << "icd9MajMinToCode: mjr.size() = " << mjr.size()
              << " and mnr.size() = " << mnr.size() << "\n";
#endif
#ifdef ICD_DEBUG
  if (mjr.size() != mnr.size())
    Rcpp::stop("mjr and mnr lengths differ");
#endif

#ifdef ICD_DEBUG_TRACE
  Rcpp::Rcout << "mjr and mnr are the same?\n";
#endif

  CV out;
  CV::iterator j = mjr.begin();
  CV::iterator n = mnr.begin();

  for (; j != mjr.end() && n != mnr.end(); ++j, ++n) {
    Rcpp::String mjrelem = *j;
    if (mjrelem == NA_STRING) {
      out.push_back(NA_STRING);
      continue;
    }
    // work around Rcpp bug with push_front: convert to string just for this
    // SOMEDAY: try to do this with C string instead
    const char* smj_c = mjrelem.get_cstring();
    std::string smj = std::string(smj_c);
    switch (strlen(smj_c)) {
    case 0:
      out.push_back(NA_STRING);
      continue;
    case 1:
      if (!icd9IsASingleVE(smj_c)) {
        smj.insert(0, "00");
      }
      break;
    case 2:
      if (!icd9IsASingleVE(smj_c)) {
        smj.insert(0, "0");
      } else {
        smj.insert(1, "0");
      }
      // default: // mjr is 3 (or more) chars already
    }
    Rcpp::String mnrelem = *n;
    if (mnrelem == NA_STRING) {
      out.push_back(smj);
      continue;
    }
    if (!isShort && mnrelem != "") {
      smj.append(".");
    }
    smj.append(mnrelem);
    out.push_back(smj);

  }
  return out;
}

// [[//Rcpp::export]]
void icd9ShortToParts_alt_CppStd(VecStr icd9Short,
                                 std::string mnrEmpty,
                                 VecStr &mjr,
                                 VecStr &mnr) {
  for (VecStr::size_type i = 0; i != icd9Short.size(); ++i) {
    Str s = icd9Short[i];

    s = strimCpp(s); // in place or rewrite?
    std::string::size_type sz = s.size();

    if (!icd9IsASingleE(s.c_str())) { // not an E code
      switch (sz) {
      case 1:
      case 2:
      case 3:
        mjr[i] = s.substr(0, sz);
        mnr[i] = mnrEmpty;
        continue;
      case 4:
      case 5:
        mjr[i] = s.substr(0, 3);
        mnr[i] = s.substr(3, sz - 3);
        continue;
      default:
        mjr[i] = "";
      mnr[i] = "";
      continue;
      } // switch size

      return;
    } // not E code

    // E code
    switch (sz) {
    case 2:
    case 3:
    case 4:
      mjr[i] = s.substr(0, sz);
      mnr[i] = mnrEmpty;
      break;
    case 5:
      mjr[i] = s.substr(0, 4);
      mnr[i] = s.substr(4, 1);
      break;
    default:
      mjr[i] = "";
    mnr[i] = "";
    continue;
    }
  } // for
}
