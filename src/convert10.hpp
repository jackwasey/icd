#ifndef CONVERT10_H_
#define CONVERT10_H_

#include <Rcpp.h>

Rcpp::List icd10ShortToParts(const Rcpp::CharacterVector & x, const Rcpp::String & mnrEmpty);
Rcpp::List icd10DecimalToParts(const Rcpp::CharacterVector & x, const Rcpp::String & mnrEmpty);

#endif /* CONVERT10_H_ */
