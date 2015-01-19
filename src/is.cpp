// generate header files in 'inst/include'
// [[Rcpp::interfaces(r, cpp)]]
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

//' @name icd9Is
//' @title are the given codes numeric, V or E type?
//' @description Quickly find V or E codes, without any validation.
//' @template icd9-any
//' @export
// [[Rcpp::export]]
std::vector<bool> icd9IsV(std::vector< std::string > icd9) { return icd9IsA(icd9, "Vv"); }

//' @rdname icd9Is
//' @export
// [[Rcpp::export]]
std::vector<bool> icd9IsE(std::vector< std::string > icd9) { return icd9IsA(icd9, "Ee"); }

//' @rdname icd9Is
//' @export
// [[Rcpp::export]]
std::vector<bool> icd9Iicd9E(std::vector< std::string > icd9) { return icd9IsA(icd9, "VvEe"); }

//' @rdname icd9Is
//' @export
// [[Rcpp::export]]
std::vector<bool> icd9IsN(std::vector< std::string > icd9) { return icd9IsA(icd9, "VvEe", true); }
