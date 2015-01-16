// [[Rcpp::depends(BH)]]
// and in R: Sys.setenv("PKG_LIBS"="-lboost_regex") but ?windows
#include <boost/algorithm/string.hpp>
#include <Rcpp.h>
#include <string>
using namespace Rcpp;

// [[Rcpp::export]]
bool icd9IsA_cpp_slow(std::string s, std::string c) {
  return s.find_first_of(c) != std::string::npos;
}

// [[Rcpp::export]]
std::vector<bool> icd9IsV_cpp_slower(std::vector< std::string > sv) {
  int len = sv.size();
  std::vector<bool> out(len);
  for (int i = 0; i < len; ++i) {
    std::string svt = boost::algorithm::trim_left_copy(sv[i]); // little speed difference
    if (svt.size() == 0) { continue; }
    out[i] = svt.at(0) == 'V' || svt.at(0) == 'v';
  }
  return out;
}

std::vector<bool> icd9IsA(std::vector< std::string > sv, std::string ms) {
  int len = sv.size();
  std::vector<bool> out(len);
  for (int i = 0; i < len; ++i) {
    //std::string svt = boost::algorithm::trim_left_copy(sv[i]); // little speed difference
    //if (svt.size() == 0) { continue; }
    //out[i] = svt.find_first_of("Vv") != std::string::npos;
    out[i] = sv[i].find_first_of(ms) != std::string::npos;
  }
  return out;
}

bool icd9IsSingle(std::string s, std::string ms) {
    return s.find_first_of(ms) != std::string::npos;
}
// [[Rcpp::export]]
bool icd9IsSingleV(std::string s) {
  return icd9IsSingle(s, "Vv");
}
// [[Rcpp::export]]
bool icd9IsSingleE(std::string s) {
  return icd9IsSingle(s, "Ee");
}
// [[Rcpp::export]]
bool icd9IsSingleVE(std::string s) {
  return icd9IsSingle(s, "VvEe");
}

// [[Rcpp::export]]
std::vector<bool> icd9IsV(std::vector< std::string > sv) { return icd9IsA(sv, "Vv"); }
// [[Rcpp::export]]
std::vector<bool> icd9IsE(std::vector< std::string > sv) { return icd9IsA(sv, "Ee"); }
// [[Rcpp::export]]
std::vector<bool> icd9IsVE(std::vector< std::string > sv) { return icd9IsA(sv, "VvEe"); }

//' @name icd9Is_cpp_slow
//' @title is the given code V or E type?
//' @description quickly find V or E codes, without fully validating V or E
//'   codes. Use fixed instead of regex for speed. Don't check position of V or
//'   E: this is not validation, just a quick classification of pre-validated
//'   codes. TODO: this doesn't look efficient, but can't use fixed with ignore
//'   case, and regex slower.
//' @template icd9-any
//' @examples
//'
//' library(microbenchmark)
//' # regex is a little faster than fixed
//' icd9 <- rep(times = 500, c("1", "not", "V10.0", " E950", ""))
//' microbenchmark(times = 3,
//'   grepl(pattern = "E", icd9, fixed = TRUE) |
//'   grepl(pattern = "e", icd9, fixed = TRUE) |
//'   grepl(pattern = "V", icd9, fixed = TRUE) |
//'   grepl(pattern = "v", icd9, fixed = TRUE))
//' microbenchmark(times = 3, grepl(pattern = "[EeVv]", rnd))
//' microbenchmark(icd9:::icd9IsV_cpp_slower(icd9), icd9:::icd9IsV_R(icd9), icd9:::icd9IsV_cpp_slow(icd9), icd9:::icd9IsV(icd9))
//'
// [[Rcpp::export]]
std::vector<bool> icd9Is_cpp_slow(std::vector< std::string > sv, std::string c) {
  int len = sv.size();
  std::vector<bool> out(len);
  for (int i = 0; i < len; ++i) {
    out[i] = icd9IsA_cpp_slow(sv[i], c);
  }
  return out;
}

// [[Rcpp::export]]
std::vector<bool> icd9IsV_cpp_slow(std::vector< std::string > sv) { return icd9Is_cpp_slow(sv, "Vv");}

// [[Rcpp::export]]
std::vector<bool> icd9IsE_cpp_slow(std::vector< std::string > sv) { return icd9Is_cpp_slow(sv, "Ee");}

// [[Rcpp::export]]
std::vector<bool> icd9IsVE_cpp_slow(std::vector< std::string > sv) { return icd9Is_cpp_slow(sv, "VvEe");}
