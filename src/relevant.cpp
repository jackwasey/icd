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

typedef std::pair<std::string, int> RelPair;

void Relevant::buildCodeSetCV(const CV& codes) {
  // over-reserve (and maybe expand), target is unique number
  allCodesSet.reserve(allCodesSet.size() + codes.size());
  DEBUG_VEC(codes);
  for (String c : codes) {
    if (c != NA_STRING) { allCodesSet.insert(c.get_cstring()); }
  }
}

void Relevant::buildCodeSetInt(const IntegerVector& codes) {
  // over-reserve (and maybe expand), target is unique number
  allCodesSet.reserve(allCodesSet.size() + codes.size());
  DEBUG_VEC(codes);
  for (R_xlen_t i = 0; i != codes.size(); ++i) {
    const auto& ci = codes[i];
    const auto& cs = std::to_string(ci);
    if (!IntegerVector::is_na(ci)) { allCodesSet.insert(cs); }
  }
}

void Relevant::buildCodeSet(const SEXP& codes) {
  switch (TYPEOF(codes)) {
  case INTSXP: {
    if (!Rf_isFactor(codes)) {
      buildCodeSetInt(codes);
      break;
    }
    CV code_levs = ((IntegerVector)codes).attr("levels");
    DEBUG_VEC(code_levs);
    buildCodeSetCV(code_levs);
    break;
  }
  case STRSXP: {
    buildCodeSetCV(codes);
    break;
  }
  case VECSXP: {
    for (SEXP listItem : (List)codes) { buildCodeSet(listItem); }
    break;
  }
  default: {
    stop("Invalid type of codes to build set in Relevant");
  }
  }
}

// do the finding
CV Relevant::findRelevant() {
  for (CV cmb : src_map) {
    for (String cmbCode : cmb) {
      if (allCodesSet.find(cmbCode.get_cstring()) != allCodesSet.end()) {
        TRACE("Pushing back" << cmbCode.get_cstring());
        r.insert(((String)cmbCode).get_cstring());
      }
    }
  }
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

RelMap Relevant::findRel(const Rcpp::CharacterVector& x) {
  RelMap out;
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
