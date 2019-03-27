#include "icd_types.h"
#include "local.h"
#include <string> // for string

using namespace Rcpp;

//' @title Internal function to simplify a comorbidity map by only including
//'   codes which are parents, or identical to, a given list of codes.
//' @description Specifically, this is useful for ICD-10 codes where there are a
//' huge number of possible codes, but we do not want to make a comorbidity map
//' with such a large number of codes in it.
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
//' if (requireNamespace("icd.data", quietly = TRUE)) {
//' umap <- icd:::simplify_map_lex(icd.data::uranium_pathology$icd10, icd10_map_ahrq)
//' head(icd:::categorize_simple(icd.data::uranium_pathology, icd10_map_ahrq,
//'                       id_name = "case", code_name = "icd10"))
//' head(icd:::categorize_simple(icd.data::uranium_pathology, umap,
//'                              id_name = "case", code_name = "icd10"))
//' }
//' @keywords internal
// [[Rcpp::export(simplify_map_lex)]]
Rcpp::List simplifyMapLexicographic(const CV &pt_codes, const List map) {
  std::string ptCode;
  size_t searchLen;
  size_t pos;
  size_t cmb_len;
  // hmm, would be nice to only scan the pt_codes once, but I don't want to
  // write my own hash map code....
  CV icd_codes = unique(pt_codes);
  DEBUG_VEC(icd_codes);
  std::vector<std::unordered_set<std::string>> newMapStd(map.length());
  for (R_xlen_t i = 0; i != icd_codes.size(); ++i) {
    ptCode = icd_codes[i];
    TRACE("i = " << i << ", and ptCode = " << ptCode);
    size_t codeLen = ptCode.length();
    if (codeLen < 3) continue; // cannot be a valid ICD-10 code
    TRACE("code len >=3 chars");
    for (R_xlen_t j = 0; j < map.size(); ++j) {
      TRACE("cmb, j = " << j);
      const CV &cmbCodes = map[j];
      for (R_xlen_t k = 0; k != cmbCodes.length(); ++k) {
        cmb_len = cmbCodes[k].size();
        // if map code is longer than the patient's code, it'll never match
        if (cmb_len > codeLen) continue; // TODO: or != ???
        // we always have at least 3 characters, so spare looping inside strcmp
        TRACE("cmbCodes[k][0] = " << cmbCodes[k][0]);
        TRACE("cmbCodes[k][1] = " << cmbCodes[k][1]);
        TRACE("cmbCodes[k][2] = " << cmbCodes[k][2]);
        TRACE("ptCode[k][0] = " << ptCode[0]);
        TRACE("ptCode[k][1] = " << ptCode[1]);
        TRACE("ptCode[k][2] = " << ptCode[2]);
        if (cmbCodes[k][0] != ptCode[0] || cmbCodes[k][1] != ptCode[1] ||
            cmbCodes[k][2] != ptCode[2])
          goto no_match;
        searchLen = std::min(cmb_len, codeLen);
        TRACE("searchLen = " << searchLen);
        pos = 3;
        while (pos != searchLen) {
          if (cmbCodes[k][pos] != ptCode[pos]) {
            TRACE("mismatch, going to no_match");
            goto no_match;
          }
          ++pos;
        }
        // push the patient's ICD code, not the original comorbidity ICD code
        // onto the new map.
        newMapStd[j].insert(ptCode);
        TRACE("Going to next comorbidity");
        goto next_comorbidity;
      no_match:;
      } // end of codes in current comorbidity
    next_comorbidity:;
    } // each comorbidity
    DEBUG("finished a comorbidity");
  } // each row of input data
  List newMap = List::create();
  for (const auto &cmbSet : newMapStd) {
    CV cmbOut;
    for (const auto &cmbCode : cmbSet) { cmbOut.push_back(cmbCode); }
    cmbOut.attr("class") = ((CV)map[0]).attr("class");
    newMap.push_back(cmbOut);
  }
  newMap.attr("names") = map.attr("names");
  return newMap;
}
