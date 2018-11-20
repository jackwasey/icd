#include "icd_types.h"
#include "local.h"
#include "fastIntToString.h"
#include <algorithm>                   // for binary_search, copy
#include <vector>                      // for vector, vector<>::const_iterator
#include <unordered_set>
#include "refactor.h"
#include <string>
#include <cstring>
#include "relevant.h"

using namespace Rcpp;

void Relevant::buildCodeSetCV(const CV& codes) {
  // over-reserve (and maybe expand), target is unique number
  allCodesSet.reserve(allCodesSet.size() + codes.size());
  DEBUG_VEC(codes);
  for (String c : (CV) codes) {
    if (c != NA_STRING) {
      allCodesSet.insert(c.get_cstring());
    }
  }
}

void Relevant::buildCodeSet(const SEXP& codes) {
  switch (TYPEOF(codes)) {
  case INTSXP: {
    if (!Rf_isFactor(codes)) {
    stop("Integer vector, but not factor in Relevant.");
  }
    CV code_levs = ((IntegerVector) codes).attr("levels");
    DEBUG_VEC(code_levs);
    buildCodeSetCV(code_levs);
    break;
  }
  case STRSXP: {
    buildCodeSetCV(codes);
    break;
  }
  case VECSXP: {
    for (SEXP listItem : (List) codes) {
    buildCodeSet(listItem);
  }
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
        TRACE("Pushing back " << cmbCode);
        r.insert(((String) cmbCode).get_cstring());
      }
    }
  }
  return(wrap(r)); // or keep as STL container?
}

// setup find based on a data frame, list or vector
CV Relevant::findRelevant(const SEXP& codes) {
  buildCodeSet(codes);
  findRelevant();
  return(wrap(r)); // or keep as STL container?
}

// setup find based on some columns in a data frame
CV Relevant::findRelevant(const List& data, CV code_fields) {
  IntegerVector cols = match(code_fields, (CV) data.names());
  if (cols.size() == 0) return(CV::create());
  if (any(is_na(cols))) stop("Relevant: column names not found in data frame");
  //r.reserve(1); // TODO: reserve an acceptable size
  //for (CV::iterator ci = code_fields.begin(); ci != code_fields.end(); ++ci)
  //std::string code_field = (String) *ci;
  for (auto col : cols) {
    buildCodeSet(data[col - 1]);
  }
  findRelevant();
  return(wrap(r)); // or keep as STL container?
}
