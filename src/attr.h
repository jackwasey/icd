#ifndef ATTR_H_
#define ATTR_H_

#include "icd_types.h"

void setDecimalDiag(Rcpp::RObject& x, bool value);
void setDecimalDiag(CV& x, bool);
void setShortDiag(Rcpp::RObject& x, bool value);
void setShortDiag(CV& x, bool);

#endif /* ATTR_H_ */
