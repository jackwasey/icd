/*
 * ranges.h
 *
 *  Created on: Mar 7, 2015
 *      Author: jack
 */

#ifndef RANGES_H_
#define RANGES_H_

#ifndef Rcpp_hpp
#include <Rcpp.h>
#endif

Rcpp::CharacterVector icd9ChildrenShortCpp(Rcpp::CharacterVector icd9Short, bool onlyReal = true);
Rcpp::CharacterVector icd9ChildrenDecimalCpp(Rcpp::CharacterVector icd9Decimal, bool onlyReal = true);

#endif /* RANGES_H_ */
