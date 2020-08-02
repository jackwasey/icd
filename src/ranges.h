#ifndef RANGES_H_
#define RANGES_H_

#include "range-const.h"

Rcpp::CharacterVector icd9ExpandMinor(const std::string& mnr, bool isE = false);
Rcpp::CharacterVector icd9ChildrenShortUndefined(const Rcpp::CharacterVector& x);
Rcpp::CharacterVector icd9ChildrenShortDefined(const Rcpp::CharacterVector& x,
                                               const std::vector<std::string>& defined);
Rcpp::CharacterVector icd9ChildrenShort(const Rcpp::CharacterVector& x,
                                        const std::vector<std::string>& defined,
                                        bool leaf = true);
Rcpp::CharacterVector icd9ChildrenShortUnorderedUndefined(const Rcpp::CharacterVector& x);
Rcpp::CharacterVector icd9ChildrenShortUnorderedDefined(const Rcpp::CharacterVector& x,
                                                        const std::vector<std::string>& defined);
Rcpp::CharacterVector icd9ChildrenShortUnordered(const Rcpp::CharacterVector& x,
                                                 const std::vector<std::string>& defined,
                                                 bool leaf = true);
Rcpp::CharacterVector icd9ChildrenDecimal(const Rcpp::CharacterVector& x,
                                          const std::vector<std::string>& defined,
                                          bool leaf = true);
Rcpp::CharacterVector icd9ChildrenDecimalUnordered(const Rcpp::CharacterVector& x,
                                                   const std::vector<std::string>& defined,
                                                   bool leaf);
Rcpp::CharacterVector icd9Children(const Rcpp::CharacterVector& icd9,
                                   bool isShort,
                                   const std::vector<std::string>& defined,
                                   bool leaf = true);
#endif /* RANGES_H_ */
