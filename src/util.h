// Copyright (C) 2014 - 2018  Jack O. Wasey
//
// This file is part of icd.
//
// icd is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// icd is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with icd. If not, see <http://www.gnu.org/licenses/>.

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
