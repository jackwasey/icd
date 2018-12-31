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

#include "local.h"
#include "util.h"
#include <stdlib.h>
#include <math.h>                              // for floor
#include <stdio.h>                             // for sprintf
#include <string.h>                            // for strcmp
#include <algorithm>                           // for copy, sort, transform
#include <iterator>                            // for back_insert_iterator
#include <ostream>
#include <string>
#include <vector>

using namespace Rcpp;

// trim one string from right
std::string trimRightCpp(std::string s) {
  // Could go much faster (but less portable) with C strings. see is.cpp for
  // similar code. Only change if demonstrated as significant in benchmark
  std::size_t n = s.find_last_not_of(" \f\n\r\t\v");
  s.erase(n + 1);
  return s;
}

// trim one string from left
// [[Rcpp::export]]
std::string trimLeftCpp(std::string s) {
  std::size_t n = s.find_first_not_of(" \f\n\r\t\v");
  s.erase(0, n);
  return s;
}

// trim a single string at both ends, but loses any encoding attributes.
// [[Rcpp::export]]
std::string strimCpp(std::string s) {
  // according to
  // http://stackoverflow.com/questions/10789740/passing-stdstring-by-value-or-reference
  // C++11 will avoid copy even without using reference argument.
  return trimLeftCpp(trimRightCpp(s));
}

// [[Rcpp::export]]
VecStr trimCpp(VecStr sv) {
  for (VecStr::iterator i = sv.begin(); i != sv.end(); ++i)
    *i = strimCpp(*i);
  return sv;
}

bool icd9CompareStrings(std::string a, std::string b) {
  const char * acs = a.c_str();
  const char * bcs = b.c_str();
  // most common is numeric, so deal with that first:
  if (*acs < 'A')
    return strcmp(acs, bcs) < 0;
  // if the second char is now  a number, then we can immediately return false
  if (*bcs < 'A')
    return false;
  // V vs E as first or both characters is next
  if (*acs == 'V' && *bcs == 'E')
    return true;
  if (*acs == 'E' && *bcs == 'V')
    return false;
  // now cover both V codes or both E codes
  return strcmp(acs, bcs) < 0;
}

bool icd9ComparePair(pas a, pas b) {
  std::string af = a.first;
  std::string bf = b.first;
  return icd9CompareStrings(af, bf);
}

// add one because R indexes from 1, not 0
inline std::size_t getSecondPlusOne(const std::pair<std::string,
                                    std::size_t>& p) {
  return p.second + 1;
}

// [[Rcpp::export(icd9_order_cpp)]]
std::vector<std::size_t> icd9OrderCpp(VecStr x) {
  std::vector<std::pair<std::string, std::size_t> > vp;
  std::vector<std::size_t> out;
  out.reserve(x.size());
  vp.reserve(x.size());
  for (std::size_t i = 0; i != x.size(); ++i)
    vp.push_back(std::make_pair(x[i], i));
  std::sort(vp.begin(), vp.end(), icd9ComparePair);
  std::transform(vp.begin(), vp.end(), std::back_inserter(out),
                 getSecondPlusOne);
  return out;
}

template <int RTYPE>
IntegerVector matchFastTemplate(const Vector<RTYPE>& x,
                                const Vector<RTYPE>& table) {
  return(match(x, table));
}

// # nocov start

//' @title Faster match
//' @name match_rcpp
//' @keywords internal
// [[Rcpp::export(match_rcpp)]]
SEXP matchFast(SEXP x, SEXP table) {
  switch( TYPEOF(x) ) {
  case INTSXP: return matchFastTemplate<INTSXP>(x, table);
  case REALSXP: return matchFastTemplate<REALSXP>(x, table);
  case STRSXP: return matchFastTemplate<STRSXP>(x, table);
  }
  return R_NilValue;
}

template <int RTYPE>
LogicalVector inFastTemplate(const Vector<RTYPE>& x,
                             const Vector<RTYPE>& table) {
  return(!is_na(match(x, table)));
}

//' @describeIn match_rcpp Use faster matching for %in% equivalent.
//' @keywords internal
// [[Rcpp::export(fin)]]
SEXP inFast(SEXP x, SEXP table) {
  switch( TYPEOF(x) ) {
  case INTSXP: return inFastTemplate<INTSXP>(x, table);
  case REALSXP: return inFastTemplate<REALSXP>(x, table);
  case STRSXP: return inFastTemplate<STRSXP>(x, table);
  }
  return R_NilValue;
}

// # nocov end
