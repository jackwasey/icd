// include boost
// [[Rcpp::depends(BH)]]

// generate header files in 'inst/include'
// [[Rcpp::interfaces(r, cpp)]]
//#[[Rcpp::depends(icd9)]]

#include <boost/algorithm/string/trim.hpp>
#include <Rcpp.h>
#include <string>
#include <icd9.h>
using namespace Rcpp;

std::vector<bool> icd9IsA(std::vector< std::string > sv, std::string ms, bool inverse = false) {
  int len = sv.size();
  std::vector<bool> out(len);
  for (int i = 0; i < len; ++i) {
    //std::string svt = boost::algorithm::trim_left_copy(sv[i]); // little speed difference
    //if (svt.size() == 0) { continue; }
    //out[i] = svt.find_first_of("Vv") != std::string::npos;
    out[i] = inverse == (sv[i].find_first_of(ms) == std::string::npos);
  }
  return out;
}

// [[Rcpp::export]]
bool icd9IsASingleV(std::string s) {
  return s.find_first_of("Vv") != std::string::npos;
}

// [[Rcpp::export]]
bool icd9IsASingleE(std::string s) {
  return s.find_first_of("Ee") != std::string::npos;
}

// [[Rcpp::export]]
bool icd9IsASingleVE(std::string s) {
  return s.find_first_of("VvEe") != std::string::npos;
}

// [[Rcpp::export]]
std::vector<bool> icd9IsV(std::vector< std::string > sv) { return icd9IsA(sv, "Vv"); }
// [[Rcpp::export]]
std::vector<bool> icd9IsE(std::vector< std::string > sv) { return icd9IsA(sv, "Ee"); }
// [[Rcpp::export]]
std::vector<bool> icd9IsVE(std::vector< std::string > sv) { return icd9IsA(sv, "VvEe"); }
// [[Rcpp::export]]
std::vector<bool> icd9IsN(std::vector< std::string > sv) { return icd9IsA(sv, "VvEe", true); }
