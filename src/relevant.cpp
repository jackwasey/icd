#include "relevant.h"
#include "fastIntToString.h"
#include "icd_types.h"
#include "local.h"
#include "refactor.h"
#include <algorithm> // for binary_search, copy
#include <cstring>
#include <string>
#include <unordered_set>
#include <vector>

using namespace Rcpp;

void Relevant::buildCodeSetCV(const Rcpp::CharacterVector& codes) {
  // over-reserve (and maybe expand), target is unique number
  ICD_TIME_BEGIN(relcvalloc);
  allCodesSet.reserve(allCodesSet.size() + codes.size());
  ICD_TIME_END(relcvalloc);
  DEBUG_VEC(codes);
  ICD_TIME_BEGIN(relcvloop);
  for (String c : codes) {
#ifndef PTR_SET
    // if I (hopefully correctly) assume uniqueness of the C string pointers that R has already made
    // unique in the global cache, nearly 50% faster on this bottleneck step.
    allCodesSet.insert((US_T)c.get_cstring());
#else
    if (c != NA_STRING) {
      // insert is quicker than emplace when dumping in Rcpp::String - 2.7s vs 3.5s for a million
      // rows
      allCodesSet.insert(c);
    }
#endif /* PTR_SET */
  }
#ifndef SET_PTR
  // do I even need to remove the NA_STRING at the end
  // allCodesSet.erase(allCodesSet.find(NA_STRING));
#endif
  ICD_TIME_END(relcvloop);
}

void Relevant::buildCodeSetInt(const IntegerVector& codes) {
  // over-reserve (and maybe expand), target is unique number
  ICD_TIME_BEGIN(relint);
  allCodesSet.reserve(allCodesSet.size() + codes.size());
  DEBUG_VEC(codes);
  for (R_xlen_t i = 0; i != codes.size(); ++i) {
    const auto& ci = codes[i];

#ifndef PTR_SET
    if (!IntegerVector::is_na(ci)) {
      String s(ci);
      allCodesSet.insert((US_T)s.get_cstring());
    }
#else
    const auto& cs = std::to_string(
      ci); // if a new string, need to mkCharLenCE in order to assume const char * is unique.
    if (!IntegerVector::is_na(ci)) { allCodesSet.insert(cs); }
#endif
  }
  ICD_TIME_END(relint);
}

void Relevant::buildCodeSet(const SEXP& codes) {
  switch (TYPEOF(codes)) {
  case INTSXP: {
    if (!Rf_isFactor(codes)) {
      ICD_TIME_BEGIN(relnotfctr);
      buildCodeSetInt(codes);
      ICD_TIME_END(relnotfctr);
      break;
    }
    ICD_TIME_BEGIN(relisfctr);
    CV code_levs = ((IntegerVector)codes).attr("levels");
    DEBUG_VEC(code_levs);
    buildCodeSetCV(code_levs);
    ICD_TIME_END(relisfctr);
    break;
  }
  case STRSXP: {
    ICD_TIME_BEGIN(relstr);
    buildCodeSetCV(codes);
    ICD_TIME_END(relstr);
    break;
  }
  case VECSXP: {
    ICD_TIME_BEGIN(rellist);
    for (SEXP listItem : (List)codes) { buildCodeSet(listItem); }
    ICD_TIME_END(rellist);
    break;
  }
  default: {
    stop("Invalid type of codes to build set in Relevant");
  }
  }
}

// do the finding
CV Relevant::findRelevant() {
  ICD_TIME_BEGIN(relfind);
  for (CV cmb : src_map) {
    for (String cmbCode : cmb) {
#ifndef SET_PTR
      if (allCodesSet.find((US_T)cmbCode.get_cstring()) != allCodesSet.end()) {
        TRACE("Pushing back" << cmbCode.get_cstring());
        r.insert(((US_T)cmbCode.get_cstring()));
      }
#else
      if (allCodesSet.find(cmbCode) != allCodesSet.end()) {
        TRACE("Pushing back" << cmbCode.get_cstring());
        r.insert(cmbCode);
      }
#endif /* SET_PTR */
    }
  }
  ICD_TIME_END(relfind);
  return wrap(r); // or keep as STL container?
}

// # nocov start

// setup find based on a data frame, list or vector
CV Relevant::findRelevant(const SEXP& codes) {
  buildCodeSet(codes);
  findRelevant();
  return wrap(r); // or keep as STL container, or even both?
}

// # nocov end

// setup find based on some columns in a data frame
CV Relevant::findRelevant(const List& data, const CV& code_fields) {
  IntegerVector cols = match(code_fields, (CV)data.names());
  if (cols.size() == 0) return (CV::create());
  if (any(is_na(cols))) stop("Relevant: column names not found in data frame");
  // auto len = ((VectorBase)data[1]).size();
  // r.reserve(); // Very rough heuristic
  for (auto col : cols) { buildCodeSet(data[col - 1]); }
  findRelevant();
  return wrap(r); // or keep as STL container?
}

IcdRelMap Relevant::findRel(const Rcpp::CharacterVector& x) {
  IcdRelMap out;
  DEBUG("building um using:");
  DEBUG_VEC(x);
  for (CV::const_iterator rit = x.cbegin(); rit != x.cend(); ++rit) {
    const char* code = *rit;
    DEBUG(std::distance(x.cbegin(), rit));
    DEBUG(code);
    out.insert(RelPair(code, std::distance(x.cbegin(), rit)));
  }
  return out;
}
