#ifndef CONVERT_H_
#define CONVERT_H_

#include <Rcpp.h>

// need default argument here for other functions to exploit,
// but this is then not exported by Rcpp (which works on the function body).
Rcpp::CharacterVector icd9PartsToShort(const Rcpp::List &parts);
Rcpp::CharacterVector icd9PartsToDecimal(const Rcpp::List &parts);
Rcpp::List majMinToParts(const Rcpp::CharacterVector &mjr, const Rcpp::CharacterVector &mnr);
Rcpp::List icd9DecimalToParts(const Rcpp::CharacterVector &icd9Decimal, const Rcpp::String mnr_empty);
Rcpp::List icd9ShortToParts(const Rcpp::CharacterVector &icd9Short, const Rcpp::String mnr_empty);
Rcpp::CharacterVector icd9DecimalToShort(const Rcpp::CharacterVector &icd9Decimal);
Rcpp::CharacterVector icd9ShortToDecimal(const Rcpp::CharacterVector &icd9Short);
Rcpp::CharacterVector icd9GetMajor(const Rcpp::CharacterVector &icd9, const bool isShort);

#endif /* CONVERT_H_ */
