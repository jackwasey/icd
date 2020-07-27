#ifndef IS_H_
#define IS_H_
#include "icd_types.h" // for VecStr


inline bool icd9IsASingleV(const char *s) {
  // TODO: can we just one integer math calculation to find chars between ASCII
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

std::vector<bool> icd9_is_n_rcpp(const VecStr &sv);
std::vector<bool> icd9_is_v_rcpp(const VecStr &sv);
std::vector<bool> icd9_is_e_rcpp(const VecStr &sv);

#endif /* IS_H_ */
