#ifndef UTIL_H_
#define UTIL_H_

#include "icd_types.h"
#include "local.h"
#include <utility> // for pair

std::string trimLeftCpp(std::string s);
std::string strimCpp(std::string s);
int valgrindCallgrindStart(bool zerostats);
int valgrindCallgrindStop();
bool strVecEqual(Rcpp::CharacterVector x, Rcpp::CharacterVector y);
Rcpp::CharacterVector icd10cmSort(const Rcpp::CharacterVector &x);
Rcpp::IntegerVector icd10cmOrder(const Rcpp::CharacterVector &x);
// concatenate a vector of vectors
template <class COCiter, class Oiter>
void my_concat(COCiter start, COCiter end, Oiter dest) {
  while (start != end) {
    dest = std::copy(start->begin(), start->end(), dest);
    ++start;
  }
}

inline Rcpp::CharacterVector getDataFrameStringRow(
  const Rcpp::DataFrame &df, const R_xlen_t i, const R_xlen_t start_idx = 0) {
  Rcpp::CharacterVector out;
  const auto dfCols = df.size();
  for (R_xlen_t j = start_idx; j != dfCols; ++j) {
    const Rcpp::CharacterVector &col = df(j);
    const Rcpp::String s             = col(i);
    out.push_back(s);
  }
  return out;
}
#endif /* UTIL_H_ */
