#ifndef UTIL_H_
#define UTIL_H_

#include "icd_types.h"
#include "local.h"
#include <utility>                      // for pair

typedef std::pair<std::string, std::size_t> pas;
std::string trimLeftCpp(std::string s);
std::string strimCpp(std::string s);
int valgrindCallgrindStart(bool zerostats);
int valgrindCallgrindStop();
bool icd9CompareStrings(std::string a, std::string b);
std::vector<std::size_t> icd9OrderCpp(VecStr x);
// concatenate a vector of vectors
template <class COCiter, class Oiter>
void my_concat (COCiter start, COCiter end, Oiter dest) {
  while (start != end) {
    dest = std::copy(start->begin(), start->end(), dest);
    ++start;
  }
}

#endif /* UTIL_H_ */
