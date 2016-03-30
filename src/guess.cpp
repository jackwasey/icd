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
#include "class.h"
#include "local.h"
#include <Rcpp.h>
#include <vector>
#include <string>
#include <algorithm>
#include <Rinternals.h>

//' Quickly guess whether codes are short form, as opposed to decimal 
//'
//' Very quick heuristic, ploughs through ten million codes in less than one second
//' and will stop more quickly if it finds a '.'.
//'
//' TODO: a factor version would be faster
//' @examples
//' \dontrun{
//' codes <- generate_random_short_icd9(1e7)
//' codes_factor <- factor_nosort(codes)
//' system.time(guessShortCpp(codes, 1e7))
//' codes[1] <- "100.1"
//' system.time(guessShortCpp(codes, 1e7))
//' microbenchmark::microbenchmark(icd:::icd_guess_short(codes),
//'                                icd:::guessShortCpp(codes),
//'                                icd:::guessShortPlusFactorCpp(codes),
//'                                # icd:::icd_guess_short_fast(codes_factor),
//'                                icd:::icd_guess_short_fast(codes),
//'                                times = 100L)
//' }
//' @keywords internal
// [[Rcpp::export(guess_short_cpp)]]
bool guessShortPlusFactorCpp(SEXP x_, int n = 100L) {
  Rcpp::CharacterVector x;
        switch(TYPEOF(x_)) {
            case STRSXP: {
                x = Rcpp::as<Rcpp::CharacterVector>(x_);
		break;    
            }
      	    case INTSXP: {
                if (Rf_isFactor(x_))
                x = Rf_getAttrib(x_, R_LevelsSymbol);
                break;
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
