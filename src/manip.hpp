#ifndef MANIP_H_
#define MANIP_H_

#include "icd_types.hpp"

CV icd9AddLeadingZeroes(CV icd9, bool isShort);
CV icd9AddLeadingZeroesShort(CV icd9Short);
Rcpp::String icd9AddLeadingZeroesMajorSingle(Rcpp::String major);
std::string icd9AddLeadingZeroesMajorSingleStd(std::string m);
CV icd9AddLeadingZeroesMajor(CV mjr);

#endif /* MANIP_H_ */
