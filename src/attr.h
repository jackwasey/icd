#ifndef ATTR_H_
#define ATTR_H_

#include "icd_types.h"
#include <Rcpp.h>

// void setDecimalDiag(Rcpp::RObject& x, bool value);
void setDecimalDiag(Rcpp::CharacterVector& x, bool);
// void setShortDiag(Rcpp::RObject& x, bool value);
void setShortDiag(Rcpp::CharacterVector& x, bool);

#endif /* ATTR_H_ */
