// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
using namespace Rcpp;

std::string trimRight(std::string& s) {
   std::size_t n = s.find_last_not_of(" \f\n\r\t\v");
   s.erase(n + 1);
   return s;
}

std::string trimLeft(std::string& s) {
   std::size_t n = s.find_first_not_of(" \f\n\r\t\v");
   s.erase(0, n);
   return s;
}

// [[Rcpp::export]]
std::string strim_cpp(std::string& s) {
   trimLeft(s);
   trimRight(s);
   return s;
}

// [[Rcpp::export]]
std::vector<std::string > trim_cpp(std::vector<std::string >& sv) {
  for (std::vector<std::string >::iterator i = sv.begin(); i != sv.end(); ++i) {
    strim_cpp(*i);
  }
  return sv;
}
