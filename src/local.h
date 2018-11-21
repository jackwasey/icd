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

// [[Rcpp::plugins(cpp11)]]
#ifndef LOCAL_H_
#define LOCAL_H_
#include "icd_types.h"
#include <set>
#include <unordered_map>
#include <unordered_set>
extern "C" {
#include "cutil.h"
#include <cstdlib>
}
#define ICD_DEBUG
// #define ICD_DEBUG_TRACE
// #define ICD_DEBUG_UTIL
// #define ICD_DEBUG_SETUP
// #define ICD_DEBUG_SETUP_SLOW
// #define ICD_DEBUG_SETUP_TRACE
// #define ICD_DEBUG_PARALLEL
// #define ICD_VALGRIND

#ifdef ICD_DEBUG
#define DEBUG(x) do { Rcpp::Rcout << x << std::endl; } while (0)
#else
#define DEBUG(x) ((void)0)
#endif

#ifdef ICD_DEBUG
#define DEBUG_VEC(x) do { Rcpp::Rcout << #x << ": " << std::flush; printIt(x); } while (0);
#else
#define DEBUG_VEC(x) ((void)0)
#endif

#ifdef ICD_DEBUG_TRACE
#define TRACE_VEC(x) do { Rcpp::Rcout << #x << ": " << std::flush; printIt(x); } while (0);
#else
#define TRACE_VEC(x) ((void)0)
#endif

#ifdef ICD_DEBUG_TRACE
#define TRACE(x) DEBUG(x)
#else
#define TRACE(x) ((void)0)
#endif

#ifdef ICD_DEBUG_UTIL
#define DEBUG_UTIL(x) DEBUG(x)
#define TRACE_UTIL(x) TRACE(x)
#define DEBUG_UTIL_VEC(x) DEBUG_VEC(x)
#else
#define DEBUG_UTIL(x) ((void)0)
#define TRACE_UTIL(x) ((void)0)
#define DEBUG_UTIL_VEC(x) ((void)0)
#endif

#ifdef ICD_DEBUG_VALGRIND
#define DEBUG_VALGRIND(x) DEBUG(x)
#else
#define DEBUG_VALGRIND(x) ((void)0)
#endif

#define ICD_CATCH
#ifdef NDEBUG
#undef ICD_CATCH
#endif

#if defined(ICD_VALGRIND) && defined(HAVE_VALGRIND_VALGRIND_H)
#include <valgrind/callgrind.h>
#else
#undef ICD_VALGRIND
#endif

#if (defined ICD_DEBUG || defined ICD_DEBUG_SETUP)
#include <iostream>
template <typename C>
inline void printIt(const C& c, int n = 10) {
  std::ostringstream o;
  for (int i = 0; i != std::min(n, (int) c.size()); ++i)
    o << c[i] << " ";
  o << std::endl;
  o << "Length: " << c.size() << std::endl;
  Rcpp::Rcout << o.str();
  Rcpp::Rcout.flush();
}

// template <typename V>
// inline void printIt(const V& v) {
//   std::ostringstream o;
//   o << v << std::endl;
//   Rcpp::Rcout << o.str();
//   Rcpp::Rcout.flush();
// }

template <typename C>
inline void printIt(const Rcpp::Nullable<C>& c, int n = 10) {
  if (c.isNull()) {
    Rcpp::Rcout << "NULL" << std::endl;
    return;
  }
  printIt((C)c, n);
}

template <typename F, typename S>
inline void printUm(std::unordered_map<F, S> um) {
  std::vector<F> keys;
  keys.reserve(um.size());
  std::vector<S> vals;
  vals.reserve(um.size());
  for(auto kv : um) {
    keys.push_back(kv.first);
    vals.push_back(kv.second);
  }
  Rcpp::Rcout << "Unordered map keys:" << std::endl;
  printIt(keys);
  Rcpp::Rcout << "Unordered map values:" << std::endl;
  printIt(vals);
}

#endif // end (defined ICD_DEBUG || defined ICD_DEBUG_SETUP)
#endif /* LOCAL_H_ */

