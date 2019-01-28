#include "icd_types.h"                   // for CV, VecStr
#include <iterator>                      // for advance
#include <vector>
using namespace Rcpp;

// [[Rcpp::export(icd10_children_defined_cpp)]]
CV icd10ChildrenDefined(CV &x, List lookup, IntegerVector nc) {
  CV allCodes = lookup["code"];
  IntegerVector matchesNa = match(x, allCodes);
  IntegerVector matches = matchesNa[!is_na(matchesNa)]; // R indexing
  VecStr kids;
  if (matches.length() == 0) {
    if (x.length() > 0)
      warning("None of the provided ICD-10 codes matched the lookup codes");
    return(CV(0));
  }
  kids.reserve(x.length() * 10);
  CV tmp = lookup[0];
  int last_row = tmp.length(); // zero-based index
  int check_row; // zero-based index
  int parent_len; // number of characters in original parent code
  for (int i = 0; i != matches.length(); ++i) {
    check_row = matches[i]; // check the row after the parent
    parent_len = nc[matches[i] - 1];
    while (check_row < last_row && nc[check_row] > parent_len)
      ++check_row;
    CV::iterator it = allCodes.begin();
    std::advance(it, matches[i] - 1);
    CV::iterator it2 = allCodes.begin();
    std::advance(it2, check_row);
    kids.insert(kids.end(), it, it2);
  }
  return wrap(kids);
}
