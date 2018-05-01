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

// [[Rcpp::interfaces(r, cpp)]]
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
//' Current returns a vector of \code{bool} which is not thread safe, or
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
std::vector<bool> icd9_is_n_cpp(const VecStr& sv) {
  const int len = sv.size();
  std::vector<bool> out(len);
  for (int i = 0; i < len; ++i) {
    out[i] = !icd9IsASingleVE(sv[i].c_str());
  }
  return out;
}

//' @rdname icd9_is_n_cpp
//' @keywords internal
// [[Rcpp::export]]
std::vector<bool> icd9_is_v_cpp(const VecStr& sv) {
  const int len = sv.size();
  std::vector<bool> out(len);
  for (int i = 0; i < len; ++i) {
    out[i] = icd9IsASingleV(sv[i].c_str());
  }
  return out;
}

//' @rdname icd9_is_n_cpp
//' @keywords internal
// [[Rcpp::export]]
std::vector<bool> icd9_is_e_cpp(const VecStr& sv) {
  const int len = sv.size();
  std::vector<bool> out(len);
  for (int i = 0; i < len; ++i) {
    out[i] = icd9IsASingleE(sv[i].c_str());
  }
  return out;
}

