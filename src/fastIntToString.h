#ifndef FASTINTTOSTRING_H_
#define FASTINTTOSTRING_H_

#include <Rcpp.h>
using namespace Rcpp;
std::vector<std::string> fastIntToStringStd(std::vector<int> x);
Rcpp::CharacterVector fastIntToStringRcpp(Rcpp::IntegerVector x);

#endif /* FASTINTTOSTRING_H_ */
