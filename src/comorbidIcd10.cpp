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
#include "icd_types.h"
#include "local.h"
#include "config.h"
#include "comorbidIcd10.h"
#include <string.h>                         // for strlen, strncpy
#include <string>                           // for string
#include "Rcpp.h"
extern "C" {
#include <cstddef>                          // for size_t
}

using namespace Rcpp;

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
//' umap <- icd:::simplify_map_lex(uranium_pathology$icd10, icd10_map_ahrq)
//' head(icd:::categorize_simple(uranium_pathology, icd10_map_ahrq,
//'                       id_name = "case", code_name = "icd10"))
//' head(icd:::categorize_simple(uranium_pathology, umap,
//'                              id_name = "case", code_name = "icd10"))
//' @keywords internal
// [[Rcpp::export(simplify_map_lex)]]
Rcpp::List simplifyMapLexicographic(const CV pt_codes, const Rcpp::List map) {
  std::string ptCode;
  size_t searchLen;
  size_t pos;
  size_t cmb_len;
  CV icd_codes = Rcpp::unique(pt_codes); // hmm, would be nice to only scan the pt_codes once, but I don't want to write my own hash map code....
  std::vector<std::unordered_set<std::string> > newMapStd(map.length());
  for (R_xlen_t i = 0; i != icd_codes.size(); ++i) {
    ptCode = icd_codes[i];
    size_t codeLen = ptCode.length();
    if (codeLen < 3)
      continue; // cannot be a valid ICD-10 code
// cannot break or goto in openmp loops #pragma omp parallel for private(searchLen, pos, cmb_len)
    for (R_xlen_t j = 0; j < map.size(); ++j) {
      const CV &cmbCodes = map[j];
      for (R_xlen_t k = 0; k != cmbCodes.length(); ++k) {
        cmb_len = cmbCodes[k].size();
        if (cmb_len > codeLen)
          continue; // if map code is longer than the patient's code, it'll never match
        // we always have at least three characters, so spare looping inside strcmp
        if (cmbCodes[k][0] != ptCode[0] ||
            cmbCodes[k][1] != ptCode[1] ||
            cmbCodes[k][2] != ptCode[2])
          goto no_match;
        searchLen = std::min(cmb_len, codeLen);
        pos = 3;
        while(pos != searchLen) {
          if (cmbCodes[k][pos] != ptCode[pos])
            goto no_match;
          ++pos;
        }
        // push the patient's ICD code, not the original comorbidity ICD code
        // onto the new map. This should be okay with openmp without critical
        // pragma, but goto is not.
        newMapStd[j].insert(ptCode);
        goto next_ptCode;
        no_match:
          ;
      } // end of codes in current comorbidity
    } // each comorbidity
    next_ptCode:;
  } // each row of input data
  Rcpp::List newMap = Rcpp::List::create();
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
