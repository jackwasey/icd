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
    //List c = (List) codes;
    //for (List::iterator i = c.begin(); i != c.end(); ++i) {
      //buildCodeSet(*i);
      buildCodeSet(listItem);
    }
    break;
  }
  default: {
    stop("Invalid type of codes to build set in Relevant");
  }
  }
}

CV Relevant::findRelevant(const SEXP& codes) {
  //r.reserve(10); // TODO: remove this test!
  //r.reserve(100 * src_map.size());
  r.reserve(1); // TODO: remove okay?
  buildCodeSet(codes);
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
