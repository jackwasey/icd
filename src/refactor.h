#ifndef REFACTOR_H_
#define REFACTOR_H_

#include "local.h"

using namespace Rcpp;
IntegerVector factorNoSort(const CharacterVector &x,
                           const CharacterVector &levels,
                           const bool na_rm);
Rcpp::IntegerVector refactor(const IntegerVector &x,
                             const CV &new_levels,
                             const bool exclude_na,
                             const bool validate = false);
Rcpp::IntegerVector refactor_narm(const IntegerVector &x,
                                  const CV &new_levels,
                                  const bool validate = false);
bool factorIsValid(const IntegerVector &f);
#endif /* REFACTOR_H_ */
