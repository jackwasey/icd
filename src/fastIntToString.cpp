#include <Rcpp.h>
#include <stdio.h>                      // for sprintf, size_t
#include <string>                       // for string, basic_string
#include <vector>                       // for vector, vector<>::size_type

// [[Rcpp::interfaces(r, cpp)]]

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
