#include "icd_types.hpp"
#include "local.hpp"
using namespace Rcpp;

#ifndef RELEVANT_H_
#define RELEVANT_H_

class Relevant {
private:
  US allCodesSet;
  US r;

public:
  const List &src_map;
  const CV relevant;
  std::unordered_map<std::string, int> rel;
  IHS hash;
  CV keys;
  // construct from a vector of codes only
  Relevant(const List &map, const SEXP &codes)
      : src_map(map), relevant(findRelevant(codes)), rel(findRel(relevant)),
        hash(IHS(relevant).fill()), keys(hash.keys()) {}
  // construct from a dataframe with given columns
  Relevant(const List &map, const List &data, CV code_names)
      : src_map(map), relevant(findRelevant(data, code_names)),
        rel(findRel(relevant)), hash(IHS(relevant).fill()), keys(hash.keys()) {
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
