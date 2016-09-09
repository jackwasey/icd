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
#include "ranges.h"
#include "icd_types.h"
#include "local.h"
#include <convert.h>
#include <convert_alt.h>
// manip just for add leading zeroes: TODO remove this dep
#include <manip.h>
#include <is.h>

// this is simplest just to hard-code
const CV vbase = CV::create("", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "00");
const CV vbase_e = CV::create("", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "00");
const CV v0 = CV::create("0", "00", "01", "02", "03", "04", "05", "06", "07", "08", "09");
const CV v1 = CV::create("1", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19");
const CV v2 = CV::create("2", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29");
const CV v3 = CV::create("3", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39");
const CV v4 = CV::create("4", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49");
const CV v5 = CV::create("5", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59");
const CV v6 = CV::create("6", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69");
const CV v7 = CV::create("7", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79");
const CV v8 = CV::create("8", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89");
const CV v9 = CV::create("9", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99");

const char* vbase_char[] = {"", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "00"};
const char* vbase_e_char[] = {"", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "00"};
const char* v0_char[] = {"0", "00", "01", "02", "03", "04", "05", "06", "07", "08", "09"};
const char* v1_char[] = {"1", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19"};
const char* v2_char[] = {"2", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29"};
const char* v3_char[] = {"3", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39"};
const char* v4_char[] = {"4", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49"};
const char* v5_char[] = {"5", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59"};
const char* v6_char[] = {"6", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69"};
const char* v7_char[] = {"7", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79"};
const char* v8_char[] = {"8", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89"};
const char* v9_char[] = {"9", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99"};

// horrible one-off code to pre-generate all the minor codes
CV MakeAllMinors() {
  CV vv = vbase;

  // create numbers 1 to 99 but cycle 10s first
  for (int i = 0; i < 10; ++i) {
    for (int j = 0; j < 10; ++j) {
      std::ostringstream s;
      s << j << i;
      if (i + j != 0)
        vv.push_back(s.str());
    }
  }
  return(vv);
}
// generate the lookups
const CV vv = MakeAllMinors();

const char* vv_char[] = {
  "", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "00",
  "0", "00", "01", "02", "03", "04", "05", "06", "07", "08", "09",
  "1", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
  "2", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
  "3", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
  "4", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49",
  "5", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59",
  "6", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69",
  "7", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79",
  "8", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89",
  "9", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99"
};

const VecStr vv_std(vv_char, end(vv_char));
const VecStr v0_std(v0_char, end(v0_char));
const VecStr v1_std(v1_char, end(v1_char));
const VecStr v2_std(v2_char, end(v2_char));
const VecStr v3_std(v3_char, end(v3_char));
const VecStr v4_std(v4_char, end(v4_char));
const VecStr v5_std(v5_char, end(v5_char));
const VecStr v6_std(v6_char, end(v6_char));
const VecStr v7_std(v7_char, end(v7_char));
const VecStr v8_std(v8_char, end(v8_char));
const VecStr v9_std(v9_char, end(v9_char));
const VecStr vbase_e_std(vbase_e_char, end(vbase_e_char));

const VecStr v_empty_std(0L);

// [[Rcpp::export]]
VecStr icd9ExpandMinorStd(const Str& mnr, bool isE) {
  if (!isE) {
    switch (mnr.size()) {
    case 0:
      return vv_std;
    case 1:
      switch (mnr[0]) { // use .at() for range check
      case '0':
        return v0_std;
      case '1':
        return v1_std;
      case '2':
        return v2_std;
      case '3':
        return v3_std;
      case '4':
        return v4_std;
      case '5':
        return v5_std;
      case '6':
        return v6_std;
      case '7':
        return v7_std;
      case '8':
        return v8_std;
      case '9':
        return v9_std;
      default:
        Rcpp::stop("unrecognized minor character");
      return v_empty_std;
      }
    case 2:
      return VecStr(1, mnr);
    default:
      Rcpp::stop("invalid minor in icd9ExpandMinorStd");
    return v_empty_std;
    }
  } else {
    // is E code, so minor must be just one character
    switch (mnr.size()) {
    case 0:
      return vbase_e_std;
    case 1:
      return VecStr(1, mnr);
    default:
      Rcpp::stop("invalid E code minor in icd9ExpandMinorStd");
    }
  }
}

// [[Rcpp::export]]
CV icd9ExpandMinorShim(std::string mnr, bool isE) {

  if (!isE) {
    switch (mnr.size()) {
    case 0:
      return vv;
    case 1:
      switch (mnr.at(0)) {
      case '0':
        return v0;
      case '1':
        return v1;
      case '2':
        return v2;
      case '3':
        return v3;
      case '4':
        return v4;
      case '5':
        return v5;
      case '6':
        return v6;
      case '7':
        return v7;
      case '8':
        return v8;
      case '9':
        return v9;
      default:
        Rcpp::stop("unrecognized minor character");
      return CV::create();
      }
      break;
    case 2:
      return Rcpp::wrap(mnr);
    default:
      Rcpp::stop("minor of more than two characters");
    return CV::create();
    }
  } else {
    // is E code, so minor must be just one character
    switch (mnr.size()) {
    case 0:
      return CV::create("", "0", "1", "2", "3", "4", "5",
                                           "6", "7", "8", "9");
    case 1:
      return mnr;
    default:
      Rcpp::stop("too many characters for an E code minor\n");
    }
  }
  return (NA_STRING); // should never get here
}

// [[Rcpp::export]]
CV icd9ChildrenShortCpp(CV icd9Short, bool onlyReal) {
  std::set<Str> out; // we are never going to put NAs in the output, so use std structure
  // this is a slower function, can the output set be predefined in size?
  if (icd9Short.size() != 0) {
    // TODO by reference or updating arguments instead? Unclear benefit, but
    // this does/did take a lot of cycles in valgrind
    Rcpp::List parts = icd9ShortToPartsCpp(icd9Short, "");
    CV major = parts[0];
    CV minor = parts[1];

    CV::iterator itmajor = major.begin();
    CV::iterator itminor = minor.begin();
    for (; itmajor != major.end(); ++itmajor, ++itminor) {
      std::string thismajor = Rcpp::as<std::string>(*itmajor);
      std::string thisminor = Rcpp::as<std::string>(*itminor);

      CV newminors = icd9ExpandMinorShim(thisminor,
                                                            icd9IsASingleE(thismajor.c_str()));

      std::vector<std::string> newshort = Rcpp::as<std::vector<std::string> >(
        icd9MajMinToShort(thismajor, newminors));

      out.insert(newshort.begin(), newshort.end());
    }
    if (onlyReal) {
      const Rcpp::Environment env("package:icd");
      Rcpp::List icd9Hierarchy = env["icd9cm_hierarchy"];
      std::set<Str> out_real;
      std::vector<std::string> tmp = Rcpp::as<std::vector<std::string> >(
        icd9Hierarchy["code"]);
      // 'reals' is the set of majors, intermediate and leaf codes.
      std::set<Str> reals(tmp.begin(), tmp.end());

      // set_intersection doesn't work for unordered sets
      std::set_intersection(out.begin(), out.end(),
                            reals.begin(), reals.end(),
                            std::inserter(out_real, out_real.begin()));
      out = out_real;
    }
  } // input length != 0
  CV rcppOut = Rcpp::wrap(out);
  rcppOut.attr("icd_short_diag") = true;
  return rcppOut;
}

// TODO: icd9ChildrenShortCppUnordered no NA version

// [[Rcpp::export]]
CV icd9ChildrenShortCppUnordered(CV icd9Short, bool onlyReal) {
  icd_set out; // we are never going to put NAs in the output, so use std structure
  // this is a slower function, can the output set be predefined in size?
  if (icd9Short.size() != 0) {
    Rcpp::List parts = icd9ShortToPartsCpp(icd9Short, "");
    CV major = parts[0];
    CV minor = parts[1];

    CV::iterator itmajor = major.begin();
    CV::iterator itminor = minor.begin();
    for (; itmajor != major.end(); ++itmajor, ++itminor) {
      Str thismajor = Rcpp::as<Str>(*itmajor);
      Str thisminor = Rcpp::as<Str>(*itminor);

      CV newminors = icd9ExpandMinorShim(thisminor,
                                                            icd9IsASingleE(thismajor.c_str()));

      VecStr newshort = Rcpp::as<VecStr>(icd9MajMinToShort(thismajor, newminors));
      out.insert(newshort.begin(), newshort.end());
    }
    if (onlyReal) {
      const Rcpp::Environment env("package:icd");
      Rcpp::List icd9Hierarchy = env["icd9cm_hierarchy"];
      icd_set out_real;
      std::vector<std::string> tmp = Rcpp::as<std::vector<std::string> >(
        icd9Hierarchy["code"]);
      // 'reals' is the set of majors, intermediate and leaf codes.
      icd_set reals(tmp.begin(), tmp.end());

#ifdef HAVE_CXX11
      for (icd_set::iterator j = out.begin(); j != out.end(); ++j) {
        if (reals.find(*j) != reals.end())
          out_real.insert(*j);
      }
#else
      std::set_intersection(out.begin(), out.end(),
                            reals.begin(), reals.end(),
                            std::inserter(out_real, out_real.begin()));
#endif
      out = out_real;
    }
  } // input length != 0
  CV rcppOut = Rcpp::wrap(out);
  rcppOut.attr("icd_short_diag") = true;
  return rcppOut;
}


// [[Rcpp::export]]
VecStr icd9ChildrenShortCppNoNaUnordered(VecStr icd9Short, bool onlyReal) {
  icd_set out; // we are never going to put NAs in the output, so use std structure
  // this is a slower function, can the output set be predefined in size?
  VecStr major(icd9Short.size());
  VecStr minor(icd9Short.size());
  if (icd9Short.size() != 0) {
    icd9ShortToPartsCppStd(icd9Short, "", major, minor);

    VecStr::iterator itmajor = major.begin();
    VecStr::iterator itminor = minor.begin();
    for (; itmajor != major.end(); ++itmajor, ++itminor) {
      VecStr newminors = icd9ExpandMinorStd(*itminor, icd9IsASingleE((*itmajor).c_str()));

      VecStr newshort = icd9MajMinToShortStd(*itmajor, newminors);
      out.insert(newshort.begin(), newshort.end());
    }
    if (onlyReal) {
      const Rcpp::Environment env("package:icd");
      Rcpp::List icd9Hierarchy = env["icd9cm_hierarchy"];
      icd_set out_real;
      std::vector<std::string> tmp = Rcpp::as<std::vector<std::string> >(
        icd9Hierarchy["code"]);
      // 'reals' is the set of majors, intermediate and leaf codes.
      icd_set reals(tmp.begin(), tmp.end());

#ifdef HAVE_CXX11
      for (icd_set::iterator j = out.begin(); j != out.end(); ++j) {
        if (reals.find(*j) != reals.end())
          out_real.insert(*j);
      }
#else
      std::set_intersection(out.begin(), out.end(),
                            reals.begin(), reals.end(),
                            std::inserter(out_real, out_real.begin()));
#endif
      out = out_real;
    }
  } // input length != 0
  VecStr rcppOut = Rcpp::wrap(out);
  rcppOut.attr("icd_short_diag") = true;
  return rcppOut;
}

// [[Rcpp::export]]
CV icd9ChildrenDecimalCpp(CV icd9Decimal,
                                             bool onlyReal) {
  CV shrt = icd9DecimalToShort(icd9Decimal);
  CV kids = icd9ChildrenShortCpp(shrt, onlyReal);
  CV out = icd9ShortToDecimal(kids);
  out.attr("icd_short_diag") = false;
  return out;
}

// [[Rcpp::export]]
CV icd9ChildrenCpp(CV icd9, bool isShort,
                                      bool onlyReal = true) {
  if (isShort)
    return icd9ChildrenShortCpp(icd9, onlyReal);
  return icd9ChildrenDecimalCpp(icd9, onlyReal);
}


//' @title match ICD9 codes
//' @description Finds children of \code{icd9Reference} and looks for \code{icd9} in the
//'   resulting vector.
//' @templateVar icd9AnyName "icd9,icd9Reference"
//' @template icd9-any
//' @template short_code
//' @param isShortReference logical, see argument \code{short_code}
//' @return logical vector
//' @keywords internal
// [[Rcpp::export]]
Rcpp::LogicalVector icd_in_reference_code(CV icd,
                                          CV icd_reference,
                                          bool short_code,
                                          bool short_reference = true) {

  if (!short_code)
    icd = icd9DecimalToShort(icd);

  CV y = icd9ChildrenCpp(icd_reference, short_reference, false);
  if (!short_reference)
    y = icd9DecimalToShort(y);
  // TODO: use hash/environment
  Rcpp::LogicalVector res = !is_na(match(icd, y));
  return res;
}
