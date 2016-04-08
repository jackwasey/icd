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
// [[Rcpp::plugins(openmp)]]

#include "comorbidIcd10.h"
#include "fastmatch.h"
#include "local.h"
#include <Rcpp.h>
#include <vector>
#include <string>

using Rcpp::CharacterVector;
using Rcpp::IntegerVector;
using Rcpp::LogicalMatrix;
using Rcpp::String;
using Rcpp::wrap;
using Rcpp::any;
using Rcpp::as;

//' Internal function to find ICD-10 parents
//'
//' Written in C++ for speed. Takes short code, short map, and pre-defined
//' visit_name and icd_name.
//' @param x Character vector (not factor)
//' @seealso \url{https://github.com/s-u/fastmatch/blob/master/src/fastmatch.c}
//' @keywords internal
// [[Rcpp::export]]
Rcpp::LogicalMatrix icd10_comorbid_parent_search_cpp(Rcpp::DataFrame x,
                                                     Rcpp::List map,
                                                     std::string visit_name,
                                                     std::string icd_name) {

  CharacterVector icd_codes = x[icd_name];
  LogicalMatrix intermed(icd_codes.size(), map.size()); // zero-filled

#ifdef ICD_DEBUG
  char debug_buf[10];
#endif
  // simplest code (and maybe fastest) is to calc comorbidity for each code,
  // then aggregate on patient code

  String code;
  std::size_t codeNchar; // or char? we know it'll be short
  CharacterVector oneCbd;
  // this must be longer than any code plus one for zero terminator. Some risk
  // of buffer overflow here.
  char codeCur[16];

  //char code_cstring[12]; // 12 should be enough. TOOD: test length
  // SEXP test_str = PROTECT(Rf_allocVector(STRSXP, 1));

  for (std::size_t i = 0; i != icd_codes.size(); ++i) {
    // look backwards from length of current code in characters. This seems
    // terrible, but when there are so many miniscule subdivisions of ICD-10-CM,
    // we can't easily or efficiently extrapolate all these current and future
    // possibilities without a lot of memory use (and more time searching those
    // lookups)

#ifdef ICD_DEBUG
    sprintf(debug_buf, "%u", (unsigned)i);
    Rcpp::Rcout << "icd10 cmbd working on row " << debug_buf << " of x\n";
#endif

    // char_count <- nchar(as.character(y)):3
    code = icd_codes[i];
    const char * code_cstring = code.get_cstring();
    codeNchar = strlen(code_cstring);
    if (codeNchar > 15)
      Rcpp::stop("ICD-10 codes must all be less than 16 characters long.");
    size_t codeCurChar;

    for (std::size_t j = 0; j != map.size(); ++j) {
#ifdef ICD_DEBUG
      sprintf(debug_buf, "%u", (unsigned)j);
      Rcpp::Rcout << "icd10 cmbd working on comorbidity " << debug_buf << " from map\n";
#endif
      oneCbd = map[j];
      SEXP oneCbdSexp = PROTECT(oneCbd.eval());

      // Using fastmatch: either start with full string and lop off a character
      // at a time, or start with 3 digit substring and extend until it matches.
      // At the moment, most maps have the higher level codes in, and so this
      // would be quicker.
      //
      // Or Rcpp: Rcpp::match sugar function actually uses a hashmap, but does
      // it recreate the hashmap each time? Maybe even a linear search is faster
      // sometimes as there are only a handful of comorbidities (but can be
      // many).

      // copy the const char * code to a writeable buffer
      // strcpy(codeCur, code_cstring); // copy here for the moving null terminator reverse method
      //for (codeCurChar = codeNchar; codeCurChar != 2; --codeCurChar) {
      for (codeCurChar = 3; codeCurChar != codeNchar + 1; ++codeCurChar) {
#ifdef ICD_DEBUG_TRACE
        Rcpp::Rcout << "codeNchar = " << codeNchar << ", ";
        Rcpp::Rcout << "codeCurChar = " << codeCurChar << ", ";
#endif
        strncpy(codeCur, code_cstring,  codeCurChar); // write codeCurChar chars to codeCur
        codeCur[codeCurChar] = '\0'; // place the null terminator
#ifdef ICD_DEBUG_TRACE
        Rcpp::Rcout << "codeCur = " << codeCur << "\n";
#endif
        // SET_STRING_ELT(test_str, 0, Rf_mkChar(codeCur));
        SEXP test_str = PROTECT(Rf_mkString(codeCur));
        // TODO we can customize fast match to just 'find' and not match every term
#ifdef ICD_DEBUG_TRACE
        CharacterVector s_debug(test_str);
        Rcpp::Rcout << "test_str = " << ((String)s_debug[0]).get_cstring() << "\n";
#endif


        //IntegerVector fm_out(fmatch(test_str, oneCbdSexp, IntegerVector(0).eval()));
        //if (fm_out[0] != 0) {
        if (ffind_one_str(test_str, oneCbdSexp) != 0) {
#ifdef ICD_DEBUG_TRACE
          Rcpp::Rcout << "found\n";
#endif
          intermed(i, j) = true; // the rest are zero filled
          UNPROTECT(1);
          // we've found the comorbidity for the current code, so break out of
          // comorbidity loop
          goto got_comorbid;
        }
        UNPROTECT(1);
      } // for chars in a single code
#ifdef ICD_DEBUG_TRACE
      Rcpp::Rcout << "not found\n";
#endif
      got_comorbid: // just placeholder for the goto target
        UNPROTECT(1);
    } // each comorbidity
  } // each row of input data

  // aggregate: probably a better way to write this special case by hand...
  // Rcpp::Function aggregate("aggregate.data.frame");
  // res <- aggregate(x = t(just_cmb), by = x[visit_name], FUN = any)
  //aggregate.data.frame()

   // UNPROTECT(1); // test_str
  // haven't aggregated yet, so there is a row for each row of the input data
  intermed.attr("dimnames") = Rcpp::List::create(x[visit_name], map.names());

  return intermed;
}
