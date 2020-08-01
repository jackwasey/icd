#include "icd_types.h"
#include "local.h"
#include <iterator> // for advance
#include <vector>

typedef Rcpp::CharacterVector CharacterVector;
typedef Rcpp::IntegerVector IntegerVector;
typedef Rcpp::List List;

// [[Rcpp::export(icd10_children_defined_rcpp)]]
CharacterVector icd10ChildrenDefined(const CharacterVector& x,
                                     const List& lookup,
                                     const IntegerVector& nc,
                                     const bool warn = true) {
  if (!lookup.containsElementNamed("code")) { Rcpp::stop("lookup does not have a code column"); }
  const CharacterVector& allCodes = lookup["code"];
  if (nc.size() != allCodes.size()) {
    DEBUG_VEC(nc);
    DEBUG_VEC(allCodes);
    Rcpp::stop("nc is not the same length as allCodes!");
  }
  const IntegerVector matchesNa = match(x, allCodes);
  const IntegerVector matches   = matchesNa[!is_na(matchesNa)]; // R indexing
  VecStr kids;
  if (matches.length() == 0) {
    if (warn && x.length() > 0)
      Rcpp::warning("None of the provided ICD-10 codes matched the lookup codes");
    return (CharacterVector(0));
  }
  kids.reserve(x.length() * 10);
  CharacterVector tmp = lookup[0];
  int last_row        = tmp.length(); // zero-based index
  int check_row;                      // zero-based index
  int parent_len;                     // number of characters in original parent code
  TRACE("Ready to loop in icd10ChildrenDefined");
  for (int i = 0; i != matches.length(); ++i) {
    check_row  = matches[i]; // check the row after the parent
    parent_len = nc[matches[i] - 1];
    while (check_row < last_row && nc[check_row] > parent_len) ++check_row;
    CharacterVector::const_iterator it = allCodes.begin();
    std::advance(it, matches[i] - 1);
    CharacterVector::const_iterator it2 = allCodes.begin();
    std::advance(it2, check_row);
    kids.insert(kids.end(), it, it2);
  }
  TRACE("Returning kids from icd10ChildrenDefined");
  DEBUG_VEC(kids);
  return Rcpp::wrap(kids);
}
