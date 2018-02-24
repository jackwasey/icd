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
#include <Rcpp/r/headers.h>                  // for NA_STRING, R_xlen_t, Rf_...
#include <string.h>                          // for strlen
#include <algorithm>                         // for fill
#include <iterator>                          // for distance
#include <string>                            // for basic_string, operator!=
#include <vector>                            // for vector, vector<>::iterator
#include "Rcpp.h"                            // for wrap
#include "Rcpp/String.h"                     // for String, string_proxy::op...
#include "Rcpp/vector/Vector.h"              // for Vector<>::const_iterator
#include "Rcpp/vector/const_string_proxy.h"  // for const_string_proxy
#include "RcppCommon.h"                      // for Proxy_Iterator
#include "icd_types.h"                       // for VecStr, CV, Str
#include "is.h"                              // for icd9IsASingleVE

//' Convert \code{mjr} and \code{mnr} vectors to single code
//'
//' In debug mode, will check that \code{mjr} and \code{mnr} are same length.
//' @template mjr
//' @template mnr
//' @template isShort
//' @examples
//' \dontrun{
//' n <- 5e6
//' mjrs <- as.character(sample(1:999, n, replace = TRUE))
//' mnrs <- as.character(sample(0:99, n, replace = TRUE))
//' microbenchmark::microbenchmark(
//'   icd9MajMinToCode(mjrs, mnrs, TRUE),
//'   icd9MajMinToCodeStd(mjrs, mnrs, TRUE),
//'   icd9MajMinToCodePrePadded(mjrs, mnrs, TRUE),
//'   times = 10
//' )
//' }
//' # std method about the same with O3 (4% faster, but no NA handling), but 50% quicker with O0
//' # std method without doing padding is 5 times quicker than previous...
//' @keywords internal manip
// [[Rcpp::export]]
CV icd9MajMinToCode(const CV mjr,
                                       const CV mnr,
                                       bool isShort) {
#ifdef ICD_DEBUG_TRACE
  Rcpp::Rcout << "icd9MajMinToCode: mjr.size() = " << mjr.size()
              << " and mnr.size() = " << mnr.size() << "\n";
#endif
#ifdef ICD_DEBUG
  if (mjr.size() != mnr.size())
    Rcpp::stop("mjr and mnr lengths differ");
#endif

  VecStr out(mjr.size());
  std::vector<char> out_is_na(mjr.size()); // boolean in char
  CV::const_iterator j = mjr.begin();
  CV::const_iterator n = mnr.begin();

  for (; j != mjr.end() && n != mnr.end(); ++j, ++n) {
    Rcpp::String mjrelem = *j;
    if (CV::is_na(*j)) {
      out_is_na[std::distance(mjr.begin(), j)] = 1;
      continue;
    }
    const char* smj_c = mjrelem.get_cstring();
    Str smj = std::string(smj_c);
    switch (strlen(smj_c)) {
    case 0:
      out_is_na[std::distance(mjr.begin(), j)] = 1;
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
    if (CV::is_na(*n)) {
      mnrelem = "";
    }
    if (!isShort && mnrelem != "") {
      smj.append(".");
    }
    smj.append(mnrelem);
    out[std::distance(mjr.begin(), j)] = smj;
  }
  CV r_out = Rcpp::wrap(out);
#ifdef ICD_DEBUG_TRACE
  Rcpp::Rcout << "NA loop size: " << out_is_na.size() << "\n";
#endif
  for (std::vector<char>::iterator i = out_is_na.begin(); i != out_is_na.end(); ++i) {
#ifdef ICD_DEBUG_TRACE
    Rcpp::Rcout << "NA loop: " << std::distance(out_is_na.begin(), i) << "\n";
#endif
    if (*i == 0)
      continue;
    r_out[std::distance(out_is_na.begin(), i)] = NA_STRING;
  }
  return r_out;
}

//' @describeIn icd9MajMinToCode Same as \code{icd9MajMinToCode} but assume
//' codes are already trimmed and correctly padded with zeros, e.g. E001, V09,
//' 001. This version does handle NA values correctly. ' @keywords internal
//[[Rcpp::export]]
CV icd9MajMinToCodePrePadded(const CV mjr,
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
VecStr icd9MajMinToCodeStd(const VecStr& mjr, const VecStr& mnr, bool isShort) {
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
CV icd9MajMinToShort(const CV mjr,
                     const CV mnr) {
#ifdef ICD_DEBUG_TRACE
  Rcpp::Rcout << "icd9MajMinToShort: mjr.size() = " << mjr.size()
              << " and mnr.size() = " << mnr.size() << "\n";
#endif
#ifdef ICD_DEBUG
  if ((mjr.size() != 1 && mjr.size() != mnr.size())
        || (mjr.size() == 1 && mnr.size() == 0)) {
    Rcpp::stop(
      "icd9MajMinToShort, length of mjrs and mnrs must be equal, unless mjrs length is one.");
  }
#endif
  if (mjr.size() == 1) {
#ifdef ICD_DEBUG_TRACE
    Rcpp::Rcout << "icd9MajMinToShort: mjr.size() = 1\n";
#endif
    CV newmjr(mnr.size(), mjr[0]);
    return icd9MajMinToCode(newmjr, mnr, true);
  }
  return icd9MajMinToCode(mjr, mnr, true);
}

// [[Rcpp::export]]
CV icd9MajMinToDecimal(const CV mjr,
                                          const CV mnr) {
  return icd9MajMinToCode(mjr, mnr, false);
}

//' append minor to major using std
//'
//' benefits from having reserve string size of 5
//' @keywords internal
// [[Rcpp::export]]
void icd9AppendMinors(VecStr& m, const VecStr& mnr, bool isShort) {
  VecStr::size_type mjsz = m.size();
  VecStr::size_type j;
  for (j = 0; j != mjsz; ++j) {
    if (!isShort && mnr[j] != "")
      m[j].append(".");
    m[j].append(mnr[j]);
  }
}

// //' append minor to major using std, with reservation of string length
// //'
// //' if \code{m} string size is already reserved, then use other \code{icd9AppendMinors}
// // [[Rcpp::export]]
// void icd9AppendMinors(VecStr& m, const VecStr& mnr, bool isShort, bool reserve = true) {
//   if (reserve)
//     m.reserve(5 + (VecStr::size_type)isShort);
//   VecStr::size_type mjsz = m.size();
//   VecStr::size_type j;
//   for (j = 0; j != mjsz; ++j) {
//     if (!isShort && mnr[j] != "")
//       m[j].append(".");
//     m[j].append(mnr[j]);
//   }
// }

inline void icd9AppendMinorsShort(VecStr& m, const VecStr& mnr) {
  for (VecStr::size_type j = 0; j != m.size(); ++j) {
    m[j].append(mnr[j]);
  }
}

inline void icd9AppendMinorsShort(VecStr& m, const VecStr& mnr, bool reserve) {
  if (reserve)
    m.reserve(5);
  for (VecStr::size_type j = 0; j != m.size(); ++j) {
    m[j].append(mnr[j]);
  }
}

//' initialize a std::vector of strings with repeated value of the minor
//' @keywords internal
// [[Rcpp::export]]
VecStr icd9MajMinToShortStd(const VecStr& mjr, const VecStr& mnr) {
  if (mjr.size() == 1) {
    Str m;
    m.reserve(5);
    m = mjr[0];
    VecStr newmjr(mnr.size(), m);
    icd9AppendMinorsShort(newmjr, mnr);
    return newmjr;
  }
  return icd9MajMinToCodeStd(mjr, mnr, true);
}

// [[Rcpp::export]]
VecStr icd9MajMinToShortSingleStd(const Str& mjr, const VecStr& mnr) {
    VecStr newmjr(mnr.size(), mjr);
    return icd9MajMinToShortStd(newmjr, mnr);
}
