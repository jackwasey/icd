#include <icd_types.h>
#include <stdio.h>                      // for sprintf, size_t
#include <string>                       // for string, basic_string
#include <vector>                       // for vector, vector<>::size_type

// [[Rcpp::interfaces(r, cpp)]]

//' @title Convert integers to strings as quickly as possible
//' @description Have tried R, sprintf with Rcpp and C++ standard library.
//' @param x Vector of integers
//' @return Vector of characters
//' @keywords internal manip
// [[Rcpp::export]]
Rcpp::CharacterVector fastIntToStringRcpp(Rcpp::IntegerVector x) {
  size_t len = x.size();
  Rcpp::CharacterVector out(len);
  char buffer[32];
  for (size_t i = 0; i != len; ++i) {
    sprintf(buffer, "%u", x[i]);
    out[i] = buffer;
  }
  return out;
}
