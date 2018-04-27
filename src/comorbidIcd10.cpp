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
// [[Rcpp::plugins(openmp)]]
#include "config.h"
#include "local.h" // for DEBUG
#include "comorbidIcd10.h"
#include <string.h>                         // for strlen, strncpy
#include <string>                           // for string
#include "Rcpp.h"                           // for wrap
#include "icd_types.h"                      // for CV
extern "C" {
#include <cstddef>                          // for size_t
}

using Rcpp::IntegerVector;
using Rcpp::LogicalMatrix;
using Rcpp::String;
using Rcpp::wrap;
using Rcpp::any;
using Rcpp::as;

//' Internal function to find ICD-10 parents
//' @param x Character vector (not factor)
//' @template mapping
//' @template visit_name
//' @template icd_name
//' @seealso \url{https://github.com/s-u/fastmatch/blob/master/src/fastmatch.c}
//' @keywords internal
// [[Rcpp::export(icd10_comorbid_parent_search_cpp)]]
Rcpp::LogicalMatrix icd10ComorbidParentSearchCpp(Rcpp::DataFrame x,
                                                 Rcpp::List map,
                                                 std::string visit_name,
                                                 std::string icd_name) {
  CV icd_codes = x[icd_name];
  LogicalMatrix intermed(icd_codes.size(), map.size()); // zero-filled
  // simplest code (and maybe fastest) is to calc comorbidity for each code,
  // then aggregate on patient code
  String code;
  std::size_t codeNchar; // or char? we know it'll be short
  CV oneCbd;
  // this must be longer than any code plus one for zero terminator. Some risk
  // of buffer overflow here.
  char codeCur[16];

  for (R_xlen_t i = 0; i != icd_codes.size(); ++i) {
    // look backwards from length of current code in characters. This seems
    // terrible, but when there are so many miniscule subdivisions of ICD-10-CM,
    // we can't easily or efficiently extrapolate all these current and future
    // possibilities without a lot of memory use (and more time searching those
    // lookups)
    code = icd_codes[i];
    const char * code_cstring = code.get_cstring();
    codeNchar = strlen(code_cstring);
    if (codeNchar > 15)
      Rcpp::stop("ICD-10 codes must all be less than 16 characters long.");
    if (codeNchar <3) {
      // abort to avoid segfault. will need to do better than this and
      // replace with NA and go to next item
      //Rcpp::stop("ICD-10 codes must be at least three characters long.");
      continue;
    }
    size_t codeCurChar;
    for (R_xlen_t j = 0; j != map.size(); ++j) {
      oneCbd = map[j];
      for (codeCurChar = 3; codeCurChar != codeNchar + 1; ++codeCurChar) {
        strncpy(codeCur, code_cstring,  codeCurChar); // write codeCurChar chars to codeCur
        codeCur[codeCurChar] = '\0'; // place the null terminator
        SEXP test_str = PROTECT(Rf_mkString(codeCur));
        CV lookup_code = test_str;
        Rcpp::IntegerVector matched_indices = Rcpp::match(lookup_code, oneCbd);
        if (matched_indices[0] > 0) {
          intermed(i, j) = true; // the rest are zero filled
          // we've found the comorbidity for the current code, so break out of
          // comorbidity loop
          UNPROTECT(1);
          goto got_comorbid;
        }
        UNPROTECT(1);
      } // for chars in a single code
      got_comorbid: // just placeholder for the goto target
        ;
    } // each comorbidity
  } // each row of input data
  // haven't aggregated yet, so there is a row for each row of the input data
  intermed.attr("dimnames") = Rcpp::List::create(x[visit_name], map.names());
  return intermed;
}

//' @title Internal function to simplify a comorbidity map by only including codes
//' which are parents, or identical to, a given list of codes.
//' @description
//' Specifically, this is useful for ICD-10 codes where there are a huge number
//' of possible codes, but we do not want to make a comorbidity map with such a
//' large number of codes in it.
//' @param x Character vector (not factor)
//' @template mapping
//' @template visit_name
//' @template icd_name
//' @seealso \url{https://github.com/s-u/fastmatch/blob/master/src/fastmatch.c}
//' @examples
//' # one exact match, next cmb parent code, next cmb child code
//' icd10 <- as.icd10(c("I0981", "A520", "I26019"))
//' pts <- data.frame(visit_id = c("a", "b", "c"), icd10)
//' simple_map <- icd:::simplify_map_lex(icd10, icd10_map_ahrq)
//' stopifnot(simple_map$CHF == "I0981")
//' stopifnot(simple_map$PHTN != character(0))
//' stopifnot(simple_map$PVD == "I26019")
//'
//' umap <- icd:::simplify_map_lex(uranium_pathology$icd10, icd10_map_ahrq)
//' icd:::comorbid_common(uranium_pathology, icd10_map_ahrq,
//'                       visit_name = "case", icd_name = "icd10",
//'                       comorbid_fun = icd:::comorbidMatMul)
//'
//' @keywords internal
// [[Rcpp::export(simplify_map_lex)]]
Rcpp::List simplifyMapLexicographic(CV pt_codes, Rcpp::List map) {
  std::string ptCode;
  Rcpp::String oneCbd;
  std::string codeCur;
  CV cmbRef;
  Rcpp::List newMap;

  CV icd_codes = Rcpp::unique(pt_codes); // hmm, would be nice to only scan the pt_codes once, but I don't want to write my own hash map code....

  std::vector<std::unordered_set<std::string> > newMapStd(map.length());
  for (R_xlen_t i = 0; i != icd_codes.size(); ++i) {
    ptCode = icd_codes[i];
    size_t codeLen = ptCode.length();
    if (codeLen < 3)
      continue; // cannot be a valid ICD-10 code
    for (R_xlen_t j = 0; j != map.size(); ++j) {
      const CV cmbCodes = map[j];
      for (CV::const_iterator k = cmbCodes.cbegin(); k != cmbCodes.cend(); ++k) {
        std::string cmb = Rcpp::as<std::string>(*k);
        if (cmb.length() > codeLen)
          continue; // if map code is longer than the patient's code, it'll never match
        size_t searchLen = std::min(cmb.length(), codeLen);
        size_t pos = 0;
        while(pos != searchLen) {
          if (cmb[pos] != ptCode[pos])
            goto no_match;
          ++pos;
        }
        newMapStd[j].insert(ptCode); // push the patient's code, not the comorbidity onto the new map
        no_match:
          ;
      } // end of codes in current comorbidity
    } // each comorbidity
  } // each row of input data
  for (auto cmbSet : newMapStd) {
    CV cmbOut;
    for (auto cmbCode : cmbSet) {
      cmbOut.push_back(cmbCode);
    }
    cmbOut.attr("class") = ((CV)map[0]).attr("class");
    newMap.push_back(cmbOut);
  }
  newMap.attr("names") = map.attr("names");
  return newMap;
}
