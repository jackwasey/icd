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

#include <Rcpp.h>
#include <algorithm>                    // for set_intersection
#include <iterator>                     // for insert_iterator, inserter
#include <set>                          // for _Rb_tree_const_iterator, set
#include <vector>                       // for vector, vector<>::const_iterator
#include "appendMinor_alt.h"           // for icd9MajMinToShortSingle_alt_Std
#include "convert.h"                    // for icd9ShortToPartsCpp
#include "convert_alt.h"                // for icd9ShortToParts_alt_Cpp
//#include "icd_types.h"                  // for VecStr, CV, Str
#include "is.h"                         // for icd9IsASingleE
//#include "local.h"                      // for icd_set
// #include "ranges.h" // #include "range-const.h"                    // for v_empty_std, v0, v0_std, v1
#include "ranges_alt.h"                     // for icd9ExpandMinor_alt_Std

//' Find child codes from vector of ICD-9 codes.
//'
//' Pure C++11 implementation using \code{unordered set} to find children of
//' given codes
//' @keywords internal
// [[Rcpp::export]]
CV icd9ChildrenShort_alt_11(CV icd9Short, bool onlyReal) {
  icd_set out; // we are never going to put NAs in the output?
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
      VecStr newminors = icd9ExpandMinor_alt_Std(thisminor, icd9IsASingleE(thismajor.c_str()));
      VecStr newshort = icd9MajMinToShortSingle_alt_Std(thismajor, newminors);
      out.insert(newshort.begin(), newshort.end());
    }
    if (onlyReal) {
      const Rcpp::Environment env("package:icd");
      Rcpp::List icd9Hierarchy = env["icd9cm_hierarchy"];
      icd_set out_real;
      VecStr tmp = Rcpp::as<VecStr >(
        icd9Hierarchy["code"]);
      // 'reals' is the set of majors, intermediate and leaf codes.
      std::set<Str> reals(tmp.begin(), tmp.end());
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

//' C++ implementation of finding children of short codes
//' @keywords internal
// [[Rcpp::export]]
CV icd9ChildrenShort_alt_Std(CV icd9Short, bool onlyReal) {
  // set may be unordered_set if C++11 is available, so may have to reorder at end
  // http://www.cplusplus.com/reference/unordered_set/unordered_set/unordered_set/
  icd_set out(icd9Short.size()); // n is hash buckets, not items

  // we are never going to put NAs in the output, so use std structure this is a

  // This is a slower function, can the output set be predefined in size?
  if (icd9Short.size() != 0) {
    VecStr major(icd9Short.size());
    VecStr minor(icd9Short.size());
    icd9ShortToParts_alt_CppStd(Rcpp::as<VecStr>(icd9Short), "", major, minor);
    VecStr::const_iterator itmajor = major.begin();
    VecStr::const_iterator itminor = minor.begin();
    for (; itmajor != major.end(); ++itmajor, ++itminor) {
      Str thismajor = *itmajor;
      Str thisminor = *itminor;
      VecStr newminors = icd9ExpandMinor_alt_Std(thisminor, icd9IsASingleE(thismajor.c_str()));
      VecStr newshort = icd9MajMinToShortSingle_alt_Std(thismajor, newminors);
      out.insert(newshort.begin(), newshort.end());
    }

    if (onlyReal) {
      const Rcpp::Environment env("package:icd");
      Rcpp::List icd9Hierarchy = env["icd9cm_hierarchy"];
      icd_set out_real;
      VecStr tmp = Rcpp::as<VecStr>(icd9Hierarchy["code"]);
      // 'reals' is the set of all known, 'real' defined codes
      icd_set reals(tmp.begin(), tmp.end());
      for (icd_set::iterator j = out.begin(); j != out.end(); ++j) {
        if (reals.find(*j) != reals.end())
          out_real.insert(*j);
      }
      out = out_real;
    }
  } // input length != 0
  CV rcppOut = Rcpp::wrap(out);
  rcppOut.attr("icd_short_diag") = true;
  return rcppOut;
}

// [[Rcpp::export]]
VecStr icd9Children_alt_ShortNoNaUnordered(const VecStr& icd9Short, const bool onlyReal) {
  icd_set out; // we are never going to put NAs in the output, so use std structure
  // this is a slower function, can the output set be predefined in size?
  VecStr mjr(icd9Short.size());
  VecStr mnr(icd9Short.size());
  if (icd9Short.size() != 0) {
    icd9ShortToParts_alt_CppStd(icd9Short, "", mjr, mnr);

    VecStr::iterator itmjr = mjr.begin();
    VecStr::iterator itmnr = mnr.begin();
    for (; itmjr != mjr.end(); ++itmjr, ++itmnr) {
      const VecStr& newminors = icd9ExpandMinor_alt_Std(*itmnr, icd9IsASingleE((*itmjr).c_str()));
      VecStr newshort = icd9MajMinToShortSingle_alt_Std(*itmjr, newminors);
      out.insert(newshort.begin(), newshort.end());
    }
    if (onlyReal) {
      const Rcpp::Environment env("package:icd");
      Rcpp::List icd9Hierarchy = env["icd9cm_hierarchy"];
      icd_set out_real;
      VecStr tmp = Rcpp::as<VecStr >(
        icd9Hierarchy["code"]);
      // 'reals' is the set of majors, intermediate and leaf codes.
      icd_set reals(tmp.begin(), tmp.end());

      for (icd_set::iterator j = out.begin(); j != out.end(); ++j) {
        if (reals.find(*j) != reals.end())
          out_real.insert(*j);
      }
      out = out_real;
    }
  } // input length != 0
  // TODO in R wrapper: rcppOut.attr("icd_short_diag") = true;
  // sort from unordered set into a vector
  VecStr out_vec(out.begin(), out.end());
  return out_vec;
}

// [[Rcpp::export]]
VecStr icd9ExpandMinor_alt_Std(const Str& mnr, bool isE) {
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
      Rcpp::stop("invalid minor in icd9ExpandMinor_alt_Std");
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
      Rcpp::stop("invalid E code minor in icd9ExpandMinor_alt_Std");
    }
  }
}
