#ifndef APPENDMINOR_H_
#define APPENDMINOR_H_

#include <Rcpp.h>

Rcpp::CharacterVector
icd9MajMinToCode(Rcpp::CharacterVector mjr, Rcpp::CharacterVector mnr, const bool short_code);
Rcpp::CharacterVector icd9MajMinToShort(Rcpp::CharacterVector mjr, Rcpp::CharacterVector mnr);
Rcpp::CharacterVector icd9MajMinToDecimal(Rcpp::CharacterVector mjr, Rcpp::CharacterVector mnr);

#endif /* APPENDMINOR_H_ */
