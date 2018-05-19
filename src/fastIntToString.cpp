#include <Rcpp.h>
#include <stdio.h>                      // for sprintf, size_t
#include <string>                       // for string, basic_string
#include <vector>                       // for vector, vector<>::size_type

// [[Rcpp::interfaces(r, cpp)]]

//' @title Convert integers to strings as quickly as possible
//' @description Have tried R, sprintf with Rcpp and C++ standard library.
//' @param x Vector of integers
//' @return Vector of characters
//' @keywords internal manip
//' @rdname fastIntToString
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

//' @title Fast factor generation with hashing, no sorting
//' @description Rcpp minimal factor creation
//' @param x vector of strings
//' @param levels vector of levels
//' @keywords internal manip
// [[Rcpp::export(factor_nosort_rcpp)]]
  Rcpp::IntegerVector factorNoSort(const Rcpp::Vector<STRSXP>& x,
                                   const Rcpp::Vector<STRSXP>& levels) {
    Rcpp::IntegerVector out = match(x, levels);
    out.attr("levels") = Rcpp::as<Rcpp::CharacterVector>(levels);
    out.attr("class") = "factor";
    return out;
  }
