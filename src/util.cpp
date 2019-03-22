#include "util.h"
#include "local.h"
#include <algorithm> // for copy, sort, transform
#include <iterator>  // for back_insert_iterator
#include <math.h>    // for floor
#include <ostream>
#include <stdio.h> // for sprintf
#include <stdlib.h>
#include <string.h> // for strcmp
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
  for (VecStr::iterator i = sv.begin(); i != sv.end(); ++i) *i = strimCpp(*i);
  return sv;
}

// [[Rcpp::export(icd9_compare_rcpp)]]
bool icd9Compare(std::string a, std::string b) {
  const char *acs = a.c_str();
  const char *bcs = b.c_str();
  // most common is numeric, so deal with that first:
  if (*acs < 'A') return strcmp(acs, bcs) < 0;
  // if the second char is now  a number, then we can immediately return false
  if (*bcs < 'A') return false;
  // V vs E as first or both characters is next
  if (*acs == 'V' && *bcs == 'E') return true;
  if (*acs == 'E' && *bcs == 'V') return false;
  // now cover both V codes or both E codes
  return strcmp(acs, bcs) < 0;
}

bool icd9ComparePair(pas a, pas b) {
  std::string af = a.first;
  std::string bf = b.first;
  return icd9Compare(af, bf);
}

// add one because R indexes from 1, not 0
inline std::size_t
getSecondPlusOne(const std::pair<std::string, std::size_t> &p) {
  return p.second + 1;
}

// [[Rcpp::export(icd9_order_rcpp)]]
std::vector<std::size_t> icd9Order(VecStr x) {
  std::vector<std::pair<std::string, std::size_t>> vp;
  std::vector<std::size_t> out;
  out.reserve(x.size());
  vp.reserve(x.size());
  for (std::size_t i = 0; i != x.size(); ++i)
    vp.push_back(std::make_pair(x[i], i));
  std::sort(vp.begin(), vp.end(), icd9ComparePair);
  std::transform(vp.begin(),
                 vp.end(),
                 std::back_inserter(out),
                 getSecondPlusOne);
  return out;
}

// use for testing
bool strVecEqual(CharacterVector x, CharacterVector y) {
  if (x.size() != y.size()) {
    Rcpp::Rcout << "Lengths differ: " << x.size() << ", " << y.size()
                << std::endl;
    return false;
  }
  for (auto i = 0; i != x.size(); ++i) {
    if (x[i] != y[i]) {
      Rcpp::Rcout << "Element " << i << " differs: " << x[i] << " != " << y[i]
                  << std::endl;
      return false;
    }
  }
  return true;
}

std::vector<std::string> qx  = {"C4A", "D3A", "M1A", "Z3A", "C7A", "C7B"};
std::vector<std::string> qb  = {"C43", "D36", "M09", "Z36", "C75", "C7A"};
std::vector<std::string> qa  = {"C44", "D37", "M10", "Z37", "C7B", "C76"};
std::vector<std::string> qbb = {"C43", "D36", "M09", "Z36", "C75", "C75"};
std::vector<std::string> qaa = {"C44", "D37", "M10", "Z37", "C76", "C76"};

// bool isQuirk(std::string s) {
//   return std::find(qx.begin(), qx.end(), s.substr(0, 3))  == qx.end();
// }
//
// std::string getBeforeQuirk(std::string s) {
//   auto bqit = std::find(qx.begin(), qx.end(), s.substr(0, 3));
//   auto i = qb.begin();
//   // j is an iterator in vector "b"
//   advance(i, distance(qx.begin(), bqit));
//   return *i;
// }
//
// std::string getAfterQuirk(std::string s) {
//   auto bqit = std::find(qx.begin(), qx.end(), s.substr(0, 3));
//   auto i = qa.begin();
//   // j is an iterator in vector "b"
//   advance(i, distance(qx.begin(), bqit));
//   return *i;
// }

std::pair<bool, bool> icd10cmCompareQuirk(const Rcpp::String &x,
                                          const Rcpp::String &y,
                                          const char *quirk,
                                          const char *beforeQuirk,
                                          const char *afterQuirk,
                                          const char *beforeBeforeQuirk,
                                          const char *afterAfterQuirk);

std::pair<bool, bool> icd10cmCompareQuirk(const Rcpp::String &x,
                                          const Rcpp::String &y,
                                          const char *quirk,
                                          const char *beforeQuirk,
                                          const char *afterQuirk,
                                          const char *beforeBeforeQuirk,
                                          const char *afterAfterQuirk) {
  const char *xstr = x.get_cstring();
  const char *ystr = y.get_cstring();
  bool mx          = (x == quirk || strncmp(xstr, quirk, 3) == 0);
  bool my          = (y == quirk || strncmp(ystr, quirk, 3) == 0);
  if (!mx && !my) return std::pair<bool, bool>(false, false);
  if (x == y) return std::pair<bool, bool>(true, false);
  // if (isQuirk(beforeQuirk)) realeforeQuirk =
  // getBeforeQuirk(beforeQuirk).c_str();
  //  if (isQuirk(afterQuirk)) afterQuirk = getAfterQuirk(beforeQuirk).c_str();
  if (mx) {
    TRACE_UTIL(quirk << " matched x");
    if (my) {
      TRACE_UTIL(quirk << " also matched y for same quirk");
      return std::pair<bool, bool>(true, x < y);
    }
    TRACE_UTIL(quirk << " after x match falling through. x = " << xstr
                     << ", beforeQuirk = " << beforeQuirk
                     << ", afterQuirk = " << afterQuirk
                     << ", beforeBeforeQuirk = " << beforeBeforeQuirk
                     << ", afterAfterQuirk = " << afterAfterQuirk);

    if (strcmp(beforeQuirk, beforeBeforeQuirk)) {
      return icd10cmCompareQuirk(x,
                                 y,
                                 quirk,
                                 beforeBeforeQuirk,
                                 afterQuirk,
                                 beforeBeforeQuirk,
                                 afterAfterQuirk);
    }
    TRACE_UTIL("Didn't do X recursion");
    return std::pair<bool, bool>(true, strcmp(beforeQuirk, ystr) < 0);
  }
  TRACE_UTIL(quirk << " falling through. x = " << xstr
                   << ", afterQuirk = " << afterQuirk);
  if (strcmp(afterQuirk, afterAfterQuirk)) {
    return icd10cmCompareQuirk(x,
                               y,
                               quirk,
                               beforeQuirk,
                               afterAfterQuirk,
                               beforeBeforeQuirk,
                               afterAfterQuirk);
  }
  TRACE_UTIL("Didn't do Y recursion");
  return std::pair<bool, bool>(true, strcmp(xstr, afterQuirk) < 0);
}

// [[Rcpp::export(icd10cm_compare_rcpp)]]
bool icd10cmCompare(const Rcpp::String x, const Rcpp::String y) {
  const char *xstr = x.get_cstring();
  const char *ystr = y.get_cstring();
  const int i      = strncmp(xstr, ystr, 1);
  // get out quick if first character differs.
  if (i != 0) return i < 0;
  // in flat file, C4A is between 43 and 44. Definitive reference I am using is
  // the flat file with all the codes from CMS.
  std::pair<bool, bool> quirkResult;
  for (std::vector<std::string>::size_type j = 0; j != qa.size(); ++j) {
    TRACE_UTIL("Working on " << qx[j]);
    quirkResult = icd10cmCompareQuirk(x,
                                      y,
                                      qx[j].c_str(),
                                      qb[j].c_str(),
                                      qa[j].c_str(),
                                      qbb[j].c_str(),
                                      qaa[j].c_str());
    if (quirkResult.first) return quirkResult.second;
  }
  // int c7a_xcmp = strncmp(xstr, "C7A", 3);
  // if (x == "C7A" || c7a_xcmp == 0) {
  //   if (x == y) return false;
  //   TRACE_UTIL("x = C7A, not exact match");
  //   if (y == "C7B" || strncmp(ystr, "C7B", 3) == 0) return true;
  //   TRACE_UTIL("x = C7A, not C7B...");
  //   return y > "C75";
  // } else if (x == "C7B" || strncmp(xstr, "C7B", 3) == 0) {
  //   if (x == y) return false;
  //   int c7a_ycmp = strncmp(ystr, "C7A", 3);
  //   if (y == "C7A") {
  //     if (c7a_ycmp == 0) return false;
  //     return strcmp(xstr, ystr);
  //   }
  //
  //   return y > "C75";
  // }
  // if (y == "C7A" ||
  //     y == "C7B" ||
  //     strncmp(ystr, "C7A", 3) == 0 ||
  //     strncmp(ystr, "C7B", 3) == 0 ) {
  //   return x < "C76";
  // }
  return x < y;
}

// [[Rcpp::export(icd10cm_sort_rcpp)]]
CharacterVector icd10cmSort(const CharacterVector &x) {
  std::vector<std::string> x_ = as<std::vector<std::string> >(x);
  std::sort(x_.begin(), x_.end(), icd10cmCompare);
  return wrap(x_);
}

//' @title Order ICD-10-CM codes
//' @description currently required for C7A, C7B (which fall after C80), and
//'   D3A, which falls after D48. C4A M1A Z3A are also problems within
//'   sub-chapters.
//' @keywords internal
// [[Rcpp::export(icd10cm_order_rcpp)]]
IntegerVector icd10cmOrder(const CharacterVector &x) {
  // see icd9Order for a different approach
  CharacterVector x_sorted = icd10cmSort(x);
  return match(x, x_sorted);
}

template <int RTYPE>
IntegerVector matchFastTemplate(const Vector<RTYPE> &x,
                                const Vector<RTYPE> &table) {
  return (match(x, table));
}

// # nocov start

//' @title Faster match
//' @name match_rcpp
//' @keywords internal
// [[Rcpp::export(match_rcpp)]]
SEXP matchFast(SEXP x, SEXP table) {
  switch (TYPEOF(x)) {
  case INTSXP:
    return matchFastTemplate<INTSXP>(x, table);
  case REALSXP:
    return matchFastTemplate<REALSXP>(x, table);
  case STRSXP:
    return matchFastTemplate<STRSXP>(x, table);
  }
  return R_NilValue;
}

template <int RTYPE>
LogicalVector inFastTemplate(const Vector<RTYPE> &x,
                             const Vector<RTYPE> &table) {
  return (!is_na(match(x, table)));
}

//' @describeIn match_rcpp Use faster matching for %in% equivalent.
//' @keywords internal
// [[Rcpp::export(fin)]]
SEXP inFast(SEXP x, SEXP table) {
  switch (TYPEOF(x)) {
  case INTSXP:
    return inFastTemplate<INTSXP>(x, table);
  case REALSXP:
    return inFastTemplate<REALSXP>(x, table);
  case STRSXP:
    return inFastTemplate<STRSXP>(x, table);
  }
  return R_NilValue;
}

// # nocov end
