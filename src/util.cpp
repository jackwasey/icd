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

template <int RTYPE>
IntegerVector matchFastTemplate(const Vector<RTYPE> &x,
                                const Vector<RTYPE> &table) {
  return (match(x, table));
}

//' @title Faster match
//' @keywords internal
//' @noRd
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

//' Use faster matching for %in% equivalent
//' @keywords internal
//' @noRd
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

//' Get some information about how this DLL was compiled
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
void build_info() {
#ifdef __clang_version__
  Rcpp::Rcout << "__clang_version__ is: " << __clang_version__ << std::endl;
#endif
#ifdef __OPTIMIZE__
  Rcpp::Rcout << "__OPTIMIZE__ is set" << std::endl;
#else
  Rcpp::Rcout << "__OPTIMIZE__ unset" << std::endl;
#endif
  Rcpp::Rcout << "__VERSION__ is: " << __VERSION__ << std::endl;
#ifdef __STDC_NO_THREADS__
  Rcpp::Rcout << "__STDC_NO_THREADS__ is: " << __STDC_NO_THREADS__ << std::endl;
#else
  Rcpp::Rcout << "__STDC_NO_THREADS__ not defined, C++ STD not GNU?" << std::endl;
#endif
#ifdef __STDC_VERSION__
  Rcpp::Rcout << "__STDC_VERSION__ is: " << __STDC_VERSION__ << std::endl;
#endif
#ifdef __GLIBCXX__
  Rcpp::Rcout << "libstdc++ from GCC"  << std::endl;
#endif

#ifdef _LIBCPP_VERSION
  Rcpp::Rcout << "libc++ from LLVM/clang)" << std::endl;
#endif
#include "makevars.h"
  Rcpp::Rcout << Makevars << std::endl;
}
