#include "icd_types.h"
#include "is.h"
#include <string>
#include <vector>

bool icd9IsASingleV(const char* s) {
  while (*s == ' ')
    ++s;
  return *s == 'V' || *s == 'v';
}

bool icd9IsASingleE(const char* s) {
  while (*s == ' ')
    ++s;
  return *s == 'E' || *s == 'e';
}

bool icd9IsASingleVE(const char* s) {
  while (*s == ' ')
    ++s;
  return *s == 'V' || *s == 'E' || *s == 'v' || *s == 'e';
}

//' Do elements of vector begin with V, E (or any other character)?
//'
//' Currently returns a vector of \code{bool} which is not thread safe, or
//' particularly fast, although it is memory efficient in the standard
//' implementation. As of \code{icd9} (now known as \code{icd}) version 1.2,
//' this is not called by threaded code, but this could change, so beware! ASCII
//' spaces are trimmed from the start of the string before testing, but no other
//' white space
//' @param sv vector of strings
//' @param x \code{const char*} of choices of first character to match
//' @param invert single logical, if TRUE, negates the condition
//' @keywords internal
// [[Rcpp::export]]
std::vector<bool> icd9_is_n_rcpp(const VecStr& sv) {
  const int len = sv.size();
  std::vector<bool> out(len);
  for (int i = 0; i < len; ++i) {
    out[i] = !icd9IsASingleVE(sv[i].c_str());
  }
  return out;
}

//' @rdname icd9_is_n_rcpp
//' @keywords internal
// [[Rcpp::export]]
std::vector<bool> icd9_is_v_rcpp(const VecStr& sv) {
  const int len = sv.size();
  std::vector<bool> out(len);
  for (int i = 0; i < len; ++i) {
    out[i] = icd9IsASingleV(sv[i].c_str());
  }
  return out;
}

//' @rdname icd9_is_n_rcpp
//' @keywords internal
// [[Rcpp::export]]
std::vector<bool> icd9_is_e_rcpp(const VecStr& sv) {
  const int len = sv.size();
  std::vector<bool> out(len);
  for (int i = 0; i < len; ++i) {
    out[i] = icd9IsASingleE(sv[i].c_str());
  }
  return out;
}
