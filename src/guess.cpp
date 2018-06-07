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
#include "icd_types.h"                 // for ComorbidOut, VecVecInt, VecVec...
#include "guess.h"
#include <vector>
#include <string>
#include <algorithm>
#include <Rinternals.h>
extern "C" {
#include "cutil.h"                            // for getRListOrDfElement
}

using Rcpp::LogicalVector;

//' Guess whether codes are \code{short_code} or \code{decimal_code}
//'
//' The goal is to guess whether codes are \code{short_code} or
//' \code{decimal_code} form. Currently condense works, but not with the
//' \code{icd} look-up table currently in use. Of note, validation is a bit
//' different here, since we don't know the type until after we guess. We could
//' look for where both short_code and long are invalid, and otherwise assume
//' valid, even if the bulk are short_code. However, it may be more useful to
//' check validity after the guess.
//' @details Very quick heuristic, ploughs through ten million codes in less
//'   than one second and will stop more quickly if it finds a '.'.
//' @return single logical value, \code{TRUE} if input data are predominantly
//'   \code{short_code} type. If there is some uncertainty, then return
//'   \code{NA}.
//' @keywords internal
// [[Rcpp::export(guess_short)]]
bool guessShortCompleteCpp(SEXP x_,
                           SEXP short_code = R_NilValue,
                           int n = 1000L,
                           SEXP icd_name = R_NilValue) {
  if (!Rf_isNull(short_code))
    return Rf_asLogical(short_code);
  Rcpp::RObject isd_maybe_null = ((Rcpp::RObject)x_).attr("icd_short_diag");
  if (!isd_maybe_null.isNULL()) {
    Rcpp::LogicalVector icd_short_diag = (Rcpp::LogicalVector)isd_maybe_null;
    return icd_short_diag[0];
  }
  if (Rf_inherits(x_, "data.frame")) {
    std::string ns("icd");
    Rcpp::Function get_icd_name("get_icd_name", ns);
    Rcpp::DataFrame rdf(x_);
    if (((Rcpp::RObject)icd_name).isNULL())
      icd_name = get_icd_name(rdf, icd_name);
    SEXP icdCol = getRListOrDfElement(x_, CHAR(STRING_ELT(icd_name, 0)));
    return guessShortPlusFactorCpp(icdCol, n);
  }
  if (TYPEOF(x_) == VECSXP) {
    // don't unlist (it's complicated), just guess based on first element
    return guessShortPlusFactorCpp(VECTOR_ELT(x_, 0));
  }
  return guessShortPlusFactorCpp(x_, n);;
}

// [[Rcpp::export]]
bool guessShortPlusFactorCpp(SEXP x_, int n) {
  CV x;
  switch(TYPEOF(x_)) {
  case STRSXP: {
    x = Rcpp::as<CV>(x_);
    break;
  }
  case INTSXP: {
    if (Rf_isFactor(x_))
      x = Rf_getAttrib(x_, R_LevelsSymbol);
    break;
  }
  case LGLSXP: {
    // we will accept all logical values, if all are NA, which defauts to
    // logical unless otherwise specified. We don't know whether
    // a vector of NAs is short or decimal, Default to short.
    Rcpp::LogicalVector xl = Rcpp::LogicalVector(x_);

    if (Rcpp::all(is_na(xl)))
      return true;
    // if there were non-NA logicals, this is an error. We only looked at
    // logical vectors because a single or more NA values, are given type
    // logical by R. Don't fall through so the logical is explicit and avoid
    // compiler warning.
    Rcpp::stop("only NA_logical_ , character vectors and factors are accepted");
  }
  default: {
    Rcpp::stop("Character vectors and factors are accepted");
  }
  }
  n = std::min((int)x.length(), n);
  const char * b;
  const char * ob;
  Rcpp::String bs;
  for (R_xlen_t i = 0; i != n; ++i) {
    bs = x[i];
    b = bs.get_cstring();
    ob = b;
    while (*b) {
      if (*b == '.') return false;
      ++b;
    }
    // stop when we first get a five digit code. There are four digit major E codes.
    if ((b - ob) == 5) return true;
  }
  return true;
}
