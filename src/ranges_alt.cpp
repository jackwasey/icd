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
#include <class.h>
#include <ranges.h>
#include <convert.h>
#include <manip.h>
#include <is.h>

//' Find child codes from vector of ICD-9 codes.
//'
//' Pure C++11 implementation using unordered set to find children of given codes
//' @examples
//' \dontrun{
//' microbenchmark::microbenchmark(
//'   icd9ChildrenShortCpp(c("001", 100:500), onlyReal = TRUE),
//'   icd9ChildrenShortCpp11(c("001", 100:500), onlyReal = TRUE),
//'   times = 5)
//'   # C++11 about 15% faster for this data
//' }
//' @keywords internal
// [[Rcpp::export]]
Rcpp::CharacterVector icd9ChildrenShortCpp11(Rcpp::CharacterVector icd9Short, bool onlyReal) {
  std::unordered_set<std::string> out; // we are never going to put NAs in the output?
  // this is a slower function, can the output set be predefined in size?
  if (icd9Short.size() != 0) {
    Rcpp::List parts = icd9ShortToPartsCpp(icd9Short, "");
    Rcpp::CharacterVector major = parts[0];
    Rcpp::CharacterVector minor = parts[1];

    Rcpp::CharacterVector::iterator itmajor = major.begin();
    Rcpp::CharacterVector::iterator itminor = minor.begin();
    for (; itmajor != major.end(); ++itmajor, ++itminor) {
      std::string thismajor = Rcpp::as<std::string>(*itmajor);
      std::string thisminor = Rcpp::as<std::string>(*itminor);

      Rcpp::CharacterVector newminors = icd9ExpandMinorShim(thisminor,
                                                            icd9IsASingleE(thismajor.c_str()));

      std::vector<std::string> newshort = Rcpp::as<std::vector<std::string> >(
        icd9MajMinToShort(thismajor, newminors));

      out.insert(newshort.begin(), newshort.end());
    }
    if (onlyReal) {
      const Rcpp::Environment env("package:icd");
      Rcpp::List icd9Hierarchy = env["icd9cm_hierarchy"];
      std::unordered_set<std::string> out_real;
      std::vector<std::string> tmp = Rcpp::as<std::vector<std::string> >(
        icd9Hierarchy["code"]);
      // 'reals' is the set of majors, intermediate and leaf codes.
      std::set<std::string> reals(tmp.begin(), tmp.end());
      std::set_intersection(out.begin(), out.end(),
                            reals.begin(), reals.end(),
                            std::inserter(out_real, out_real.begin()));
      out = out_real;
    }
  } // input length != 0
  Rcpp::CharacterVector rcppOut = Rcpp::wrap(out);
  rcppOut.attr("icd_short_diag") = true;
  return rcppOut;
}
