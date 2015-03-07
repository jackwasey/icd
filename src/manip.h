/*
 * manip.h
 *
 *  Created on: Mar 7, 2015
 *      Author: jack
 */

#ifndef MANIP_H_
#define MANIP_H_



Rcpp::String icd9AddLeadingZeroesMajorSingle(Rcpp::String major);
Rcpp::CharacterVector icd9AddLeadingZeroes(Rcpp::CharacterVector icd9, bool isShort);
Rcpp::CharacterVector icd9AddLeadingZeroesShort(Rcpp::CharacterVector icd9Short);
Rcpp::CharacterVector icd9AddLeadingZeroesMajor(Rcpp::CharacterVector major);



#endif /* MANIP_H_ */
