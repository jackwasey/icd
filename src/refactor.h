#ifndef REFACTOR_H_
#define REFACTOR_H_

#include <Rcpp.h>

Rcpp::IntegerVector factorNoSort(const Rcpp::CharacterVector &x,
                           const Rcpp::CharacterVector &levels,
                           const bool na_rm);
Rcpp::IntegerVector refactor(const Rcpp::IntegerVector &x,
                             const Rcpp::CharacterVector &new_levels,
                             const bool exclude_na,
                             const bool validate = false);
Rcpp::IntegerVector refactor_narm(const Rcpp::IntegerVector &x,
                                  const Rcpp::CharacterVector &new_levels,
                                  const bool validate = false);
bool factorIsValid(const Rcpp::IntegerVector &f);
#endif /* REFACTOR_H_ */
