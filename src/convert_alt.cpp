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
#include "convert.h"
#include "local.h"
#include "util.h"
#include "is.h"
#include "manip.h"
#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::CharacterVector icd9MajMinToCodeOld(const Rcpp::CharacterVector major,
                                          const Rcpp::CharacterVector minor, bool isShort) {
#ifdef ICD_DEBUG_TRACE
  Rcpp::Rcout << "icd9MajMinToCode: major.size() = " << major.size()
              << " and minor.size() = " << minor.size() << "\n";
#endif
#ifdef ICD_DEBUG
  if (major.size() != minor.size())
    Rcpp::stop("major and minor lengths differ");
#endif

#ifdef ICD_DEBUG_TRACE
  Rcpp::Rcout << "major and minor are the same?\n";
#endif

  Rcpp::CharacterVector out; // TODO use vectors of strings and reserve space for this
  Rcpp::CharacterVector::const_iterator j = major.begin();
  Rcpp::CharacterVector::const_iterator n = minor.begin();

  for (; j != major.end() && n != minor.end(); ++j, ++n) {
    Rcpp::String mjrelem = *j;
    if (mjrelem == NA_STRING) {
      out.push_back(NA_STRING);
      continue;
    }
    // work around Rcpp bug with push_front: convert to string just for this
    // TODO: try to do this with C string instead
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
      // default: // major is 3 (or more) chars already
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
