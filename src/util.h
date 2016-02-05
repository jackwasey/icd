// Copyright (C) 2014 - 2016  Jack O. Wasey
//
// This file is part of icd9.
//
// icd9 is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// icd9 is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with icd9. If not, see <http://www.gnu.org/licenses/>.

#ifndef UTIL_H_
#define UTIL_H_

#include "local.h"
#include <Rcpp.h>
#include <vector>
#include <string>
#ifdef ICD9_OPENMP
#include <omp.h>
#endif

std::string trimLeftCpp(std::string s);
std::string strimCpp(std::string s);

int getOmpCores();
int getOmpThreads();
int getOmpMaxThreads();
void debug_parallel();
Rcpp::NumericVector randomMajorCpp(int n);
std::vector<std::string> icd9RandomShortN(std::vector<std::string>::size_type n);
std::vector<std::string> icd9RandomShortV(std::vector<std::string>::size_type n);
std::vector<std::string> icd9RandomShortE(std::vector<std::string>::size_type n);
std::vector<std::string> icd9RandomShort(std::vector<std::string>::size_type n);

std::vector<std::string> fastIntToStringStd(std::vector<int> x);
Rcpp::CharacterVector fastIntToStringRcpp(Rcpp::IntegerVector x);

int valgrindCallgrindStart(bool zerostats);
int valgrindCallgrindStop();

#endif /* UTIL_H_ */
