#ifndef APPENDMINOR_H_
#define APPENDMINOR_H_

#include <Rcpp.h>

Rcpp::CharacterVector icd9MajMinToCode(const Rcpp::CharacterVector& mjr,
                                       const Rcpp::CharacterVector& mnr,
                                       const bool short_code);
Rcpp::CharacterVector icd9MajMinToShort(const Rcpp::CharacterVector& mjr,
                                        const Rcpp::CharacterVector& mnr);
Rcpp::CharacterVector icd9MajMinToDecimal(const Rcpp::CharacterVector& mjr,
                                          const Rcpp::CharacterVector& mnr);

#endif /* APPENDMINOR_H_ */
