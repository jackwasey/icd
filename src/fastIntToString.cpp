#include <Rcpp.h>
#include <Rcpp/r/headers.h>             // for Rf_install
#include <stdio.h>                      // for sprintf, size_t
#include <string>                       // for string, basic_string
#include <vector>                       // for vector, vector<>::size_type
#include "Rcpp/vector/Vector.h"         // for Vector<>::Proxy
#include "Rcpp/vector/instantiation.h"  // for CharacterVector, IntegerVector

using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

//' @name fastIntToString
//' @title Fast convert integer vector to character vector
//' @description Fast conversion from integer vector to character vector using C++
//' @param x vector of integers
//' @param bufferSize int if any input strings are longer than this number
//'   (default 16) there will be memory errors. No checks done for speed.
//' @keywords internal
// [[Rcpp::export]]
std::vector<std::string> fastIntToStringStd(std::vector<int> x) {
  std::vector<std::string>::size_type len = x.size();
  std::vector<std::string> out(len);
  char buffer[64];
  for (std::vector<double>::size_type i = 0; i != len; ++i) {
    sprintf(buffer, "%u", x[i]);
    out[i] = buffer;
  }
  return out;
}

//' @describeIn fastIntToString Same using \code{Rcpp}
// [[Rcpp::export]]
Rcpp::CharacterVector fastIntToStringRcpp(Rcpp::IntegerVector x) {
  size_t len = x.size();
  Rcpp::CharacterVector out(len);
  char buffer[64];
  for (size_t i = 0; i != len; ++i) {
    sprintf(buffer, "%u", x[i]);
    out[i] = buffer;
  }
  return out;
}
