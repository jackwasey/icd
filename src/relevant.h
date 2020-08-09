#ifndef RELEVANT_H_
#define RELEVANT_H_

#include "icd_types.h"
#include "local.h"

typedef Rcpp::sugar::IndexHash<STRSXP> IHS;
typedef Rcpp::CharacterVector CV;
// typedef Rcpp::List List;
using Rcpp::IntegerVector;
using Rcpp::List;
class Relevant {
private:
  US allCodesSet;
  US r;

public:
  const Rcpp::List& src_map;
  const Rcpp::CharacterVector str_codes;
  std::unordered_map<std::string, int> rel_map;
  IHS hash;
  Rcpp::CharacterVector keys;
  // construct from a vector of codes only
  Relevant(const Rcpp::List& map, const SEXP& codes_in)
      : src_map(map), str_codes(findRelevant(codes_in)), rel_map(findRel(str_codes)),
        hash(IHS(str_codes).fill()), keys(hash.keys()) {}
  // construct from a dataframe with given columns
  Relevant(const List& map, const List& data, CV col_names)
      : src_map(map), str_codes(findRelevant(data, col_names)), rel_map(findRel(str_codes)),
        hash(IHS(str_codes).fill()), keys(hash.keys()) {}
  void buildCodeSetCV(const Rcpp::CharacterVector& codes);
  void buildCodeSetInt(const Rcpp::IntegerVector& codes);
  void buildCodeSet(const SEXP& codes);
  Rcpp::CharacterVector findRelevant();
  Rcpp::CharacterVector findRelevant(const SEXP& codes);
  // CV findRelevant(const DataFrame& data, CV code_fields);
  Rcpp::CharacterVector findRelevant(const Rcpp::List& data, const CV& code_names);
  RelMap findRel(const Rcpp::CharacterVector& x);
}; // Relevant

#endif // RELEVANT_H_
