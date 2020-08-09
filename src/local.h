// [[Rcpp::plugins(cpp11)]]
// [[Rcpp//plugins(openmp)]]

#ifndef LOCAL_H_
#define LOCAL_H_
#include <Rcpp.h>

//
// Useful standard and GNU extensions to C++11 and onward attributes
//
// C++ standard: https://en.cppreference.com/w/cpp/language/attributes
// clang exts: http://clang.llvm.org/docs/LanguageExtensions.html#non-standard-c-11-attributes
// gcc exts:
//
// Check has attribute available during compilatino:
// https://gcc.gnu.org/onlinedocs/cpp/_005f_005fhas_005fattribute.html

// #if defined __has_attribute
// #  if __has_attribute (nonnull)
// #    define __icd_fast_str __attribute__ ((nonnull))
// #  endif
// #endif

// #define ICD_DEBUG
// #define ICD_DEBUG_TRACE
// #define ICD_DEBUG_UTIL
// #define ICD_DEBUG_SETUP
// #define ICD_DEBUG_SETUP_SLOW
// #define ICD_DEBUG_SETUP_TRACE

#if !defined(NDEBUG)
// good for testing, but aborts all the time
//#define ICD_DEBUG
#endif /* NDEBUG */

#if defined(ICD_DEBUG) || defined(ICD_DEBUG_TRACE) || defined(ICD_DEBUG_UTIL) || \
  defined(ICD_DEBUG_SETUP) || defined(ICD_DEBUG_SETUP_SLOW) || defined(ICD_DEBUG_SETUP_TRACE)

#include <Rcpp.h>
#include <iostream> // for std::endl
#include <iostream>
#include <ostream>
#include <unordered_map>
static Rcpp::Rostream<true> so;

template <typename C> inline void printIt(const C& c, int n = 10) {
  std::ostringstream o;
  for (int i = 0; i != std::min(n, (int)c.size()); ++i) o << c[i] << " ";
  o << std::endl;
  o << "Length: " << c.size() << std::endl;
  so << o.str();
  so.flush();
}

template <typename C> inline void printIt(const Rcpp::Nullable<C>& c, int n = 10) {
  if (c.isNull()) {
    so << "NULL" << std::endl;
    return;
  }
  printIt((C)c, n);
}

template <typename F, typename S> inline void printUm(std::unordered_map<F, S> um) {
  std::vector<F> keys;
  keys.reserve(um.size());
  std::vector<S> vals;
  vals.reserve(um.size());
  for (const auto& kv : um) {
    keys.push_back(kv.first);
    vals.push_back(kv.second);
  }
  so << "Unordered map keys:" << std::endl;
  printIt(keys);
  so << "Unordered map values:" << std::endl;
  printIt(vals);
}

#define DEBUG(x) \
  do { Rcpp::Rcout << x << std::endl; } while (0)
#define DEBUG_VEC(x)                         \
  do {                                       \
    Rcpp::Rcout << #x << ": " << std::flush; \
    printIt(x);                              \
  } while (0)
#define DEBUG_VEC_SIZE(x)                    \
  do {                                       \
    Rcpp::Rcout << #x << ": " << std::flush; \
    Rcpp::Rcout << x.size() << std::endl;    \
  } while (0)

#else
#define DEBUG(x) ((void)0)
#define DEBUG_VEC(x) ((void)0)
#endif /* any ICD_ debug flag set */

#ifdef ICD_DEBUG_TRACE
#define TRACE(x) DEBUG(x)
#define TRACE_VEC(x) DEBUG_VEC(x)
#define TRACE_VEC_SIZE(x) DEBUG_VEC_SIZE(x)
#else
#define TRACE(x) ((void)0)
#define TRACE_VEC(x) ((void)0)
#define TRACE_VEC_SIZE(x) ((void)0)
#endif /* trace */

#ifdef ICD_DEBUG_UTIL
#define DEBUG_UTIL(x) DEBUG(x)
#define DEBUG_UTIL_VEC(x) DEBUG_VEC(x)
#define TRACE_UTIL(x) TRACE(x)
#else
#define DEBUG_UTIL(x) ((void)0)
#define DEBUG_UTIL_VEC(x) ((void)0)
#define TRACE_UTIL(x) ((void)0)
#endif /* util */

#endif /* LOCAL_H_ */
