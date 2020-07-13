#include "is.h"
#include "icd_types.h"
#include <string>
#include <vector>

inline bool icd9IsASingleV(const char *s) {
  // TODO: can we just one integer math calculation to find *s between ASCII
  // values 48 and 57 inclusive? (0-9)
  if ((*s <= '9' && *s >= '0') || *s == 'E') return false;
  if (*s == 'V') return true;
  while (*s == ' ') ++s;
  return *s == 'V' || *s == 'v';
}

inline bool icd9IsASingleE(const char *s) {
  if ((*s <= '9' && *s >= '0') || *s == 'V') return false;
  if (*s == 'E') return true;
    while (*s == ' ') ++s;
  return *s == 'E' || *s == 'e';
}

inline bool icd9IsASingleVE(const char *s) {
  // quickly check first character, most likely case first. Test <= '9' first
  // because lower ASCII quicker to match for number (or space)
  if (*s <= '9' && *s >= '0') return false;
  if (*s == 'V' || *s == 'E') return true;
  if (*s == 0) return false;
  while (*s == ' ') ++s; // skip spaces, surely no tabs!
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
//' @noRd
// [[Rcpp::export]]
std::vector<bool> icd9_is_n_rcpp(const VecStr &sv) {
  const int len = sv.size();
  std::vector<bool> out(len);
  for (int i = 0; i < len; ++i) { out[i] = !icd9IsASingleVE(sv[i].c_str()); }
  return out;
}

//' @title icd9_is_n_rcpp
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
std::vector<bool> icd9_is_v_rcpp(const VecStr &sv) {
  const int len = sv.size();
  std::vector<bool> out(len);
  for (int i = 0; i < len; ++i) { out[i] = icd9IsASingleV(sv[i].c_str()); }
  return out;
}

//' @title icd9_is_n_rcpp
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
std::vector<bool> icd9_is_e_rcpp(const VecStr &sv) {
  const int len = sv.size();
  std::vector<bool> out(len);
  for (int i = 0; i < len; ++i) { out[i] = icd9IsASingleE(sv[i].c_str()); }
  return out;
}
