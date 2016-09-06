// Copyright (C) 2014 - 2016  Jack O. Wasey
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
#include "util.h"
#include "local.h"
#include <vector>
#include <string>
#ifdef ICD_OPENMP
#include <omp.h>
#ifdef ICD_STD_PARALLEL
#include <parallel/algorithm>
#else
#include <algorithm>
#endif
#endif

// trim one string from right
std::string trimRightCpp(std::string s) {
  // Could go much faster (but less portable) with C strings. see is.cpp for
  // similar code. Only change if demonstrated as significant in benchmark
  std::size_t n = s.find_last_not_of(" \f\n\r\t\v");
  s.erase(n + 1);
  return s;
}

// trim one string from left
// [[Rcpp::export]]
std::string trimLeftCpp(std::string s) {
  std::size_t n = s.find_first_not_of(" \f\n\r\t\v");
  s.erase(0, n);
  return s;
}

// trim a single string at both ends, but loses any encoding attributes.
// [[Rcpp::export]]
std::string strimCpp(std::string s) {
  // according to
  // http://stackoverflow.com/questions/10789740/passing-stdstring-by-value-or-reference
  // C++11 (i.e. almost everyone) will avoid copy even without using reference
  // argument.
  return trimLeftCpp(trimRightCpp(s));
}

// [[Rcpp::export]]
std::vector<std::string> trimCpp(std::vector<std::string> sv) {
  for (std::vector<std::string>::iterator i = sv.begin(); i != sv.end(); ++i)
    *i = strimCpp(*i);
  return sv;
}

#ifdef ICD_DEBUG
void printCharVec(Rcpp::CharacterVector cv) {
  for (Rcpp::CharacterVector::iterator i=cv.begin(); i!=cv.end(); ++i) {
    Rcpp::String s = *i;
    Rcpp::Rcout << s.get_cstring() << " ";
  }
  Rcpp::Rcout << "\n";
  return;
}
#endif

// [[Rcpp::export]]
int getOmpCores() {
  int cores = 0;
#ifdef ICD_OPENMP
  cores = omp_get_num_procs();
#endif
  return cores;
}

// [[Rcpp::export]]
int getOmpMaxThreads() {
  int maxthreads = 0;
#ifdef ICD_OPENMP
  maxthreads = omp_get_max_threads();
#endif
  return maxthreads;
}

// [[Rcpp::export]]
int getOmpThreads() {
  int threads = 0;
#ifdef ICD_OPENMP
  omp_sched_t sched;
  omp_get_schedule(&sched, &threads);
#endif
  return threads;
}

void debug_parallel() {
#if defined(ICD_OPENMP) && defined(ICD_DEBUG_PARALLEL)
  Rcpp::Rcout << "threads per omp_get_schedule = " << getOmpThreads() << ". ";
  Rcpp::Rcout << "max threads per omp_get_schedule = " << getOmpMaxThreads() << ". ";
  Rcpp::Rcout << "avail threads = " << omp_get_num_threads() << ". ";
  Rcpp::Rcout << "omp_get_thread_num = " << omp_get_thread_num() << ". ";
  Rcpp::Rcout << "omp_get_num_procs = " << getOmpCores() << "\n";
#else
  Rcpp::Rcout << "ICD_OPENMP is not defined\n";
#endif
}

// [[Rcpp::export]]
Rcpp::NumericVector randomMajorCpp(int	n) {
  // This could just be a sprintf like the others.
  Rcpp::NumericVector iv = Rcpp::floor(Rcpp::runif(n) * 999);
  return iv;
}

//' @rdname icd9RandomShort
//' @keywords internal
// [[Rcpp::export]]
std::vector<std::string> icd9RandomShortN(std::vector<std::string>::size_type n = 5) {
  VecStr out(n);
  std::vector<double> randoms = Rcpp::as<std::vector<double> >(Rcpp::runif(n, 0, 99999));
  char buffer[6];
  for (std::vector<double>::size_type i = 0; i != n; ++i) {
    sprintf(buffer, "%.0f", randoms[i]);
    out[i] = buffer;
  }
  return out;
}

//' @rdname icd9RandomShort
//' @keywords internal
// [[Rcpp::export]]
std::vector<std::string> icd9RandomShortV(std::vector<std::string>::size_type n = 5) {
  VecStr out(n);
  std::vector<double> randoms = Rcpp::as<std::vector<double> >(Rcpp::runif(n, 0, 9999));
  char buffer[6];
  for (std::vector<double>::size_type i = 0; i != n; ++i) {
    sprintf(buffer, "V%.0f", randoms[i]);
    out[i] = buffer;
  }
  return out;
}

//' @rdname icd9RandomShort
//' @keywords internal
// [[Rcpp::export]]
std::vector<std::string> icd9RandomShortE(std::vector<std::string>::size_type n = 5) {
  VecStr out(n);
  std::vector<double> randoms = Rcpp::as<std::vector<double> >(Rcpp::runif(n, 0, 9999));
  char buffer[6];
  for (std::vector<double>::size_type i = 0; i != n; ++i) {
    sprintf(buffer, "E%.0f", randoms[i]);
    out[i] = buffer;
  }
  return out;
}

//' Generate random short-form ICD-9 codes
//'
//' Quick pseudo-random by picking numeric, 'V' or 'E' based on modulo three of
//' the number
//' @keywords internal
// [[Rcpp::export]]
std::vector<std::string> icd9RandomShort(std::vector<std::string>::size_type n = 5) {

  VecStr out(n);
  std::vector<double> randoms = Rcpp::as<std::vector<double> >(Rcpp::runif(n, 0, 99999));
  std::vector<double> randoms2 = Rcpp::as<std::vector<double> >(Rcpp::runif(n, 0, 3));

  char buffer[6];
  for (std::vector<char>::size_type i = 0; i != n; ++i) {
    // N, V or E?
    switch (((short) randoms2[i]) % 3) {
    case 0:
      sprintf(buffer, "%.0f", randoms[i]);
      break;
    case 1:
      sprintf(buffer, "V%.0f", randoms[i] / 10);
      break;
    case 2:
      sprintf(buffer, "E%.0f", randoms[i] / 10);
      break;
    default:
      {} // never here
    }
    out[i] = buffer;
  }
  return out;
}

//' Fast convert integer vector to character vector
//'
//' Fast conversion from integer vector to character vector using C++
//' @param x vector of integers
//' @param bufferSize int if any input strings are longer than this number
//'   (default 16) there will be memory errors. No checks done for speed.
//' @examples
//' \dontrun{
//' pts <- generate_random_pts(1e7)
//' # conclusion: buffer size matters little (so default to be more generous),
//' # and Rcpp version fastest.
//' microbenchmark::microbenchmark(fastIntToStringStd(pts$visit_id, buffer = 8),
//'                                fastIntToStringStd(pts$visit_id, buffer = 16),
//'                                fastIntToStringStd(pts$visit_id, buffer = 64),
//'                                fastIntToStringRcpp(pts$visit_id, buffer = 8),
//'                                fastIntToStringRcpp(pts$visit_id, buffer = 16),
//'                                fastIntToStringRcpp(pts$visit_id, buffer = 64),
//'                                as.character(pts$visit_id),
//'                                asCharacterNoWarn(pts$visit_id), times = 5)
//' }
//' @rdname fastIntToString
//' @keywords internal
// [[Rcpp::export]]
std::vector<std::string> fastIntToStringStd(std::vector<int> x) {
  std::vector<std::string>::size_type len = x.size();
  std::vector<std::string> out(len);
  char buffer[64];
  for (std::vector<double>::size_type i = 0; i != len; ++i) {
    sprintf(buffer, "%u", x[i]);
    out[i] = buffer;
  }
  return out;
}


//' @rdname fastIntToString
// [[Rcpp::export]]
Rcpp::CharacterVector fastIntToStringRcpp(Rcpp::IntegerVector x) {
  size_t len = x.size();
  Rcpp::CharacterVector out(len);
  char buffer[64];
  for (size_t i = 0; i != len; ++i) {
    sprintf(buffer, "%u", x[i]);
    out[i] = buffer;
  }
  return out;
}

// [[Rcpp::export]]
int valgrindCallgrindStart(bool zerostats = false) {
#ifdef ICD_VALGRIND
#ifdef ICD_DEBUG
  Rcpp::Rcout << "Starting callgrind instrumentation...\n";
#endif
  CALLGRIND_START_INSTRUMENTATION;
  if (zerostats) {
    Rcpp::Rcout << "Zeroing callgrind stats.\n";
    CALLGRIND_ZERO_STATS;
  }
#endif
  return 0;
}


// [[Rcpp::export]]
int valgrindCallgrindStop() {
#ifdef ICD_VALGRIND
#ifdef ICD_DEBUG
  Rcpp::Rcout << "Stopping callgrind instrumentation...\n";
#endif
  CALLGRIND_STOP_INSTRUMENTATION;
#endif
  return 0;
}

bool icd9CompareStrings(std::string a, std::string b) {
  const char * acs = a.c_str();
  const char * bcs = b.c_str();
  // most common is numeric, so deal with that first:
  if (*acs < 'A')
    return strcmp(acs, bcs) < 0;
  // if the second char is now  a number, then we can immediately return false
  if (*bcs < 'A')
    return false;
  // V vs E as first or both characters is next
  if (*acs == 'V' && *bcs == 'E')
    return true;
  if (*acs == 'E' && *bcs == 'V')
    return false;
  // now cover both V codes or both E codes
  return strcmp(acs, bcs) < 0;
}

bool icd9ComparePair(pas a, pas b) {
  std::string af = a.first;
  std::string bf = b.first;
  //return icd9Comp(af, bf);
  return icd9CompareStrings(af, bf);
}

// [[Rcpp::export(icd9_sort_cpp)]]
std::vector<std::string> icd9SortCpp(std::vector<std::string> x) {
  std::sort(x.begin(), x.end(), icd9CompareStrings);
  return x;
}

// add one because R indexes from 1, not 0
std::size_t getSecondPlusOne(const std::pair<std::string, std::size_t>& p) { return p.second + 1; }

// [[Rcpp::export(icd9_order_cpp)]]
std::vector<std::size_t> icd9OrderCpp(std::vector<std::string> x) {

  std::vector<std::pair<std::string, std::size_t> > vp;
  std::vector<std::size_t> out;
  out.reserve(x.size());
  vp.reserve(x.size());
  for (std::size_t i = 0; i != x.size(); ++i) {
    vp.push_back(std::make_pair(x[i], i));
  }
  std::sort(vp.begin(), vp.end(), icd9ComparePair);
  std::transform(vp.begin(), vp.end(), std::back_inserter(out), getSecondPlusOne);
  return out;
}

// //[[Rcpp::export]]
// Rcpp::CharacterVector env_to_vec_flip(Rcpp::Environment env) {
//   Rcpp::CharacterVector out(Rf_length(env.ls(true)));
// for (Rcpp::CharacterVector::iterator i; i != out.end(); ++i) {
//   out[env[*i]] = *i;
// }
// return
// }

