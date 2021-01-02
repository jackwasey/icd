#ifndef CONVERT10_H_
#define CONVERT10_H_

#include "icd_types.hpp"
using namespace Rcpp;
List icd10ShortToParts(const CV &x, const String& mnrEmpty = "");
List icd10DecimalToParts(const CV &x, const String mnrEmpty = "");

#endif /* CONVERT10_H_ */
