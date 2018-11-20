#include "icd_types.h"
#include "local.h"
#include <Rcpp.h>
using namespace Rcpp;

#ifndef RELEVANT_H_
#define RELEVANT_H_

class Relevant {
private:
  US allCodesSet;
  US r;
public:
  const List& src_map;
  const CV relevant;
  IHS hash;
  CV keys;

  Relevant(const List& map, const SEXP& codes) :
    src_map(map),
    relevant(findRelevant(codes)),
    hash(IHS(relevant).fill()),
    keys(hash.keys()) {
  }
  Relevant(const List& map, const List& data, CV code_fields) :
    src_map(map),
    relevant(findRelevant(data, code_fields)),
    hash(IHS(relevant).fill()),
    keys(hash.keys()) {
  }
  void buildCodeSetCV(const CV& codes);
  void buildCodeSet(const SEXP& codes);
  CV findRelevant();
  CV findRelevant(const SEXP& codes);
  //CV findRelevant(const DataFrame& data, CV code_fields);
  CV findRelevant(const List& data, CV code_fields);
  R_xlen_t size() { return relevant.size(); }
}; // Relevant

# endif // RELEVANT_H_
