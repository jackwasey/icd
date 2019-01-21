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

#include "icd_types.h"
#include "convert.h"
extern "C" {
#include <cstddef>                           // for size_t
}
#include <string>                            // for string
#include "Rcpp.h"                            // for wrap
#include "appendMinor.h"                     // for icd9MajMinToCode
#include "is.h"                              // for icd9IsASingleE, icd9IsAS...
#include "manip.h"                           // for icd9AddLeadingZeroesMajor
#include "util.h"                            // for strimCpp, trimLeftCpp

using namespace Rcpp;

// [[Rcpp::export]]
CV icd9PartsToShort(const List& parts) {
  CV res = icd9MajMinToCode(parts["mjr"], parts["mnr"], true);
  return res;
}

// [[Rcpp::export]]
CV icd9PartsToDecimal(const List& parts) {
  CV res = icd9MajMinToCode(parts["mjr"], parts["mnr"], false);
  return res;
}

// [[Rcpp::export]]
List majMinToParts(const CV& mjr, const CV& mnr) {
  List returned_frame = List::create(_["mjr"] = mjr,
                                     _["mnr"] = mnr);

  StringVector sample_row = returned_frame(0);
  IntegerVector row_names = seq_along(sample_row);
  returned_frame.attr("row.names") = row_names;
  // doesn't actually need a data frame, although it is barely distinguishable
  // from a list, and not costly to construct in this manner.
  returned_frame.attr("class") = "data.frame";

  return returned_frame;
}

// [[Rcpp::export]]
List icd9ShortToParts(const CV& icd9Short, String mnrEmpty) {
  CV mjr(icd9Short.size());
  CV mnr(icd9Short.size());
  for (int i = 0; i < icd9Short.size(); ++i) {
    String thisShort = icd9Short[i];
    // .is_na() is private?
    if (is_true(all(is_na(CV::create(thisShort))))) {
      mnr[i] = NA_STRING; // I think set_na() might be an alternative.
      continue;
    }
    std::string s(thisShort.get_cstring());
    s = strimCpp(s); // in place or rewrite?
    std::string::size_type sz = s.size();
    if (icd9IsASingleE(s.c_str())) { // E code
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
        // covr looks like it misses fall-through here, even though tested
        mjr[i] = NA_STRING;
      mnr[i] = NA_STRING;
      }
    } else { // not an E code
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
        mjr[i] = NA_STRING;
      mnr[i] = NA_STRING;
      }
    }
  } // for
  return majMinToParts(icd9AddLeadingZeroesMajor(mjr), mnr);
}

// [[Rcpp::export]]
List icd9DecimalToParts(const CV& icd9Decimal, const String mnrEmpty) {
  CV mjrs;
  CV mnrs;
  int ilen = icd9Decimal.length();

  if (ilen == 0) {
    return List::create(_["mjr"] =
                        CV::create(), _["mnr"] =
                        CV::create());
  }

  for (CV::const_iterator it = icd9Decimal.begin();
       it != icd9Decimal.end(); ++it) {
    String strna = *it;
    if (is_true(all(is_na(CV::create(strna)))) || strna == "") {
      mjrs.push_back(NA_STRING);
      mnrs.push_back(NA_STRING);
      continue;
    }
    // SOMEDAY, a faster way might be to use String's function
    // get_cstring, and recode the trim functions to take const char *. This
    // would avoid the type change AND may trim faster.
    std::string thiscode = as<std::string>(*it);
    thiscode = strimCpp(thiscode); // This updates 'thisccode' by reference, no copy
    std::size_t pos = thiscode.find(".");
    // substring parts
    std::string mjrin;
    String mnrout;
    if (pos != std::string::npos) {
      mjrin = thiscode.substr(0, pos);
      mnrout = thiscode.substr(pos + 1);
    } else {
      mjrin = thiscode;
      mnrout = mnrEmpty;
    }
    mjrs.push_back(icd9AddLeadingZeroesMajorSingle(mjrin));
    mnrs.push_back(mnrout);
  }
  return List::create(_["mjr"] = mjrs, _["mnr"] =
                      mnrs);
}

// [[Rcpp::export(name = "icd9_short_to_decimal_cpp")]]
CV icd9ShortToDecimal(const CV& x) {
  return icd9PartsToDecimal(icd9ShortToParts(x, ""));
}

// [[Rcpp::export(name="icd9_decimal_to_short_cpp")]]
CV icd9DecimalToShort(const CV& x) {
  CV out = clone(x); // clone instead of pushing back thousands of times
  size_t ilen = x.length();
  if (ilen == 0)
    return out;
  for (size_t i = 0; i != ilen; ++i) {
    String strna = x[i]; // need to copy here? does it copy?
    if (is_true(all(is_na(CV::create(strna)))) || strna == "")
      continue;
    const char * thiscode_cstr = strna.get_cstring();
    std::string thiscode(thiscode_cstr);
    thiscode = trimLeftCpp(thiscode);
    std::size_t pos = thiscode.find_first_of(".");
    if (pos != std::string::npos) {
      // now we assume that the mjr is snug against the left side, so we can add zero padding
      thiscode.erase(pos, 1); // remove the decimal point
      // could do fewer tests on the code by doing this last, but most codes are not V or E...
      if (pos > 0 && pos < 4 && !icd9IsASingleVE(thiscode_cstr)) {
        thiscode.insert(0, 3 - pos, '0');
      } else if (pos == 2 && icd9IsASingleV(thiscode_cstr)) {
        thiscode.insert(1, 1, '0');
        out[i] = thiscode;
      } else if ((pos == 2 || pos == 3) && icd9IsASingleE(thiscode_cstr)) {
        thiscode.insert(1, 4 - pos, '0');
      }
      // otherwise leave the code alone
      out[i] = thiscode;
    } else {
      out[i] = String(icd9AddLeadingZeroesMajorSingleStd(thiscode));
    }
  }
  return out;
}

//' @describeIn get_major Get major part of ICD-9 code, i.e. first three
//'   digits of numeric or V code, or first four digits of E code. This is the
//'   part before the decimal, when a decimal point is used.
//' @keywords internal manip
//' @export
//' @noRd
//[[Rcpp::export(name="get_major.icd9")]]
CV icd9GetMajor(const CV& x, const bool short_code) {
  if (short_code) {
    // am I casting (or just compiler/syntax checker hinting?) SEXP may be
    // costly, or is it just encapsulating a pointer to some fixed data somewhere?

    // I don't think i need to PROTECT here, because I immediately return the
    // result via Rcpp
    SEXP mjrs = icd9ShortToParts(x, "")[0]; // actually wants to be a List
    return as<CV>(mjrs);
  }
  SEXP mjrs = icd9DecimalToParts(x, "")[0];
  return as<CV>(mjrs);
}
