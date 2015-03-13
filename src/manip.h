/*
 * manip.h
 *
 *  Created on: Mar 7, 2015
 *      Author: jack
 */

#ifndef MANIP_H_
#define MANIP_H_

#include <Rcpp.h>

Rcpp::CharacterVector icd9AddLeadingZeroes(Rcpp::CharacterVector icd9, bool isShort);
Rcpp::CharacterVector icd9AddLeadingZeroesShort(Rcpp::CharacterVector icd9Short);
Rcpp::String icd9AddLeadingZeroesMajorSingleShim(Rcpp::String mjr);
Rcpp::CharacterVector icd9AddLeadingZeroesMajorShim(Rcpp::CharacterVector mjr);

#endif /* MANIP_H_ */
