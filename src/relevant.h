#include "icd_types.h"
#include "local.h"
using namespace Rcpp;

#ifndef RELEVANT_H_
#define RELEVANT_H_

class Relevant {
private:
  US allCodesSet;
  US r;

public:
  const List &src_map;
  const CV str_codes;
  std::unordered_map<std::string, int> rel_map;
  IHS hash;
  CV keys;
  // construct from a vector of codes only
  Relevant(const List &map, const SEXP &codes_in)
      : src_map(map), str_codes(findRelevant(codes_in)), rel_map(findRel(str_codes)),
        hash(IHS(str_codes).fill()), keys(hash.keys()) {}
  // construct from a dataframe with given columns
  Relevant(const List &map, const List &data, CV col_names)
      : src_map(map), str_codes(findRelevant(data, col_names)),
        rel_map(findRel(str_codes)), hash(IHS(str_codes).fill()), keys(hash.keys()) {
  }
  void buildCodeSetCV(const CV &codes);
  void buildCodeSetInt(const IntegerVector &codes);
  void buildCodeSet(const SEXP &codes);
  CV findRelevant();
  CV findRelevant(const SEXP &codes);
  // CV findRelevant(const DataFrame& data, CV code_fields);
  CV findRelevant(const List &data, const CV& code_names);
  RelMap findRel(const CharacterVector x);
}; // Relevant

#endif // RELEVANT_H_
