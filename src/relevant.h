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
  std::unordered_map<std::string, int> rel;
  IHS hash;
  CV keys;
  // construct from a vector of codes only
  Relevant(const List& map,
           const SEXP& codes) :
    src_map(map),
    relevant(findRelevant(codes)),
    rel(findRel(relevant)),
    hash(IHS(relevant).fill()),
    keys(hash.keys()) {}
  // construct from a dataframe with given columns
  Relevant(const List& map,
           const List& data,
           CV code_fields) :
    src_map(map),
    relevant(findRelevant(data, code_fields)),
    rel(findRel(relevant)),
    hash(IHS(relevant).fill()),
    keys(hash.keys()) {
    // WIP - fill unordered_map to look up an index from the code
    // TODO: build this structure INSTEAD of vector relevant codes?
    //for (CV::const_iterator rit = relevant.cbegin(); rit != relevant.cend(); ++rit)
    //  rel.insert(std::pair<std::string, int>(*rit, std::distance(relevant.cbegin(), rit)));
    //   DEBUG("retrieving test:");
    //   auto found = rel.find(((String)*rit).get_cstring());
    //   if (found != rel.end())
    //     DEBUG("found");
    //   else
    //     DEBUG("not");

  }
  void buildCodeSetCV(const CV& codes);
  void buildCodeSet(const SEXP& codes);
  CV findRelevant();
  CV findRelevant(const SEXP& codes);
  //CV findRelevant(const DataFrame& data, CV code_fields);
  CV findRelevant(const List& data, CV code_fields);
  RelMap findRel(const CharacterVector x);
}; // Relevant

# endif // RELEVANT_H_
