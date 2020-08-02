#ifndef CONVERT10_H_
#define CONVERT10_H_

#include <Rcpp.h>
typedef Rcpp::CharacterVector CV;
Rcpp::List icd10ShortToParts(const CV& x, const Rcpp::String mnr_empty = "");
Rcpp::List icd10DecimalToParts(const CV& x, const Rcpp::String mnr_empty = "");

#endif /* CONVERT10_H_ */
