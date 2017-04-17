// Copyright (C) 2014 - 2017  Jack O. Wasey
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
#include "attr.h"
#include "ranges.h"
#include "convert.h"
#include "convert_alt.h"
#include "appendMinor.h"
#include "manip.h"
#include "is.h"

//' Find child codes from vector of ICD-9 codes.
//'
//' Pure C++11 implementation using unordered set to find children of given codes
//' @examples
//' \dontrun{
//' microbenchmark::microbenchmark(
//'   icd9ChildrenShort(c("001", 100:500), onlyReal = TRUE),
//'   icd9ChildrenShort11(c("001", 100:500), onlyReal = TRUE),
//'   times = 5)
//'   # C++11 about 15% faster for this data
//' }
//' @keywords internal
// [[Rcpp::export]]
CV icd9ChildrenShort11(CV icd9Short, bool onlyReal) {
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

      VecStr newminors = icd9ExpandMinorStd(thisminor, icd9IsASingleE(thismajor.c_str()));

      VecStr newshort = icd9MajMinToShortSingleStd(thismajor, newminors);

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
//' @examples
//' \dontrun{
//' library(microbenchmark)
//' microbenchmark(icd9ChildrenShort("001", T), icd9ChildrenShortStd("001", T), times = 100)
//' microbenchmark(icd9ChildrenShort(c("001", 100:400), T),
//'                icd9ChildrenShortUnordered(c("001", 100:400), T),
//'                icd9ChildrenShortStd(c("001", 100:400), T),
//'                times = 10)
//' }
//' # unordered set much faster, but may still need to sort result
//' @keywords internal
// [[Rcpp::export]]
CV icd9ChildrenShortStd(CV icd9Short, bool onlyReal) {
  // set may be unordered_set if C++11 is available, so may have to reorder at end
#ifdef HAVE_CXX11
  // http://www.cplusplus.com/reference/unordered_set/unordered_set/unordered_set/
  icd_set out(icd9Short.size()); // n is hash buckets, not items
#else
  icd_set out; // plain std::set does not have buckets, or possiblity of reserving space.
#endif
  // we are never going to put NAs in the output, so use std structure this is a
  // slower function, can the output set be predefined in size?
  if (icd9Short.size() != 0) {
    VecStr major(icd9Short.size());
    VecStr minor(icd9Short.size());
    icd9ShortToPartsCppStd(Rcpp::as<VecStr>(icd9Short), "", major, minor);

    VecStr::const_iterator itmajor = major.begin();
    VecStr::const_iterator itminor = minor.begin();
    for (; itmajor != major.end(); ++itmajor, ++itminor) {
      Str thismajor = *itmajor;
      Str thisminor = *itminor;

      VecStr newminors = icd9ExpandMinorStd(thisminor, icd9IsASingleE(thismajor.c_str()));
      VecStr newshort = icd9MajMinToShortSingleStd(thismajor, newminors);

      out.insert(newshort.begin(), newshort.end());
    }

    if (onlyReal) {
      const Rcpp::Environment env("package:icd");
      Rcpp::List icd9Hierarchy = env["icd9cm_hierarchy"];
      icd_set out_real;
      VecStr tmp = Rcpp::as<VecStr>(icd9Hierarchy["code"]);
      // 'reals' is the set of all known, 'real' defined codes
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
