#ifndef CONVERT_H_
#define CONVERT_H_

#include "icd_types.hpp"
using namespace Rcpp;

// need default argument here for other functions to exploit,
// but this is then not exported by Rcpp (which works on the function body).
CV icd9PartsToShort(const List &parts);
CV icd9PartsToDecimal(const List &parts);
List majMinToParts(const CV &mjr, const CV &mnr);
List icd9ShortToParts(const CV &icd9Short, const String mnr_empty = "");
List icd9DecimalToParts(const CV &icd9Decimal, const String mnr_empty = "");
CV icd9DecimalToShort(const CV &icd9Decimal);
CV icd9ShortToDecimal(const CV &icd9Short);
CV icd9GetMajor(const CV &icd9, const bool isShort);

#endif /* CONVERT_H_ */
