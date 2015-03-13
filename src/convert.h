/*
 * convert.h
 *
 *  Created on: Mar 7, 2015
 *      Author: jack
 */

#ifndef CONVERT_H_
#define CONVERT_H_

#ifndef Rcpp_hpp
#include <Rcpp.h>
#endif
Rcpp::List icd9ShortToParts(const Rcpp::CharacterVector icd9Short,
		const Rcpp::String minorEmpty = "");
Rcpp::List icd9DecimalToParts(const Rcpp::CharacterVector icd9Decimal,
		const Rcpp::String minorEmpty = "");
Rcpp::CharacterVector icd9PartsToShort(const Rcpp::List parts);
Rcpp::CharacterVector icd9PartsToDecimal(const Rcpp::List parts);
Rcpp::CharacterVector icd9MajMinToShortShim(const Rcpp::CharacterVector mjr,
		const Rcpp::CharacterVector mnr);
Rcpp::CharacterVector icd9MajMinToDecimalShim(const Rcpp::CharacterVector mjr,
		const Rcpp::CharacterVector mnr);
Rcpp::CharacterVector icd9DecimalToShort(
		const Rcpp::CharacterVector icd9Decimal);
Rcpp::CharacterVector icd9ShortToDecimal(const Rcpp::CharacterVector icd9Short);

#endif /* CONVERT_H_ */
