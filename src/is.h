#ifndef IS_H_
#define IS_H_
#include "icd_types.h" // for VecStr

bool icd9IsASingleV(const char *s);
bool icd9IsASingleE(const char *s);
bool icd9IsASingleVE(const char *s);
std::vector<bool> icd9_is_n_rcpp(const VecStr &sv);
std::vector<bool> icd9_is_v_rcpp(const VecStr &sv);
std::vector<bool> icd9_is_e_rcpp(const VecStr &sv);

#endif /* IS_H_ */
