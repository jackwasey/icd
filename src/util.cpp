// Copyright (C) 2014 - 2015  Jack O. Wasey
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

// [[Rcpp::interfaces(r, cpp)]]
#include "util.h"
#include "local.h"
#include <Rcpp.h>
#include <vector>
#include <string>
#ifdef ICD9_OPENMP
#include <omp.h>
#ifdef ICD9_STD_PARALLEL
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

// [[Rcpp::export]]
bool assertFactorOrCharacter(SEXP x) {
  if (!Rf_isString(x) && !Rf_isFactor(x)) {
    Rcpp::stop("Must be a factor or character");
  }
  return true;
}

#ifdef ICD9_DEBUG
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
#ifdef ICD9_OPENMP
  cores = omp_get_num_procs();
#endif
  return cores;
}

// [[Rcpp::export]]
int getOmpMaxThreads() {
  int maxthreads = 0;
#ifdef ICD9_OPENMP
  maxthreads = omp_get_max_threads();
#endif
  return maxthreads;
}

// [[Rcpp::export]]
int getOmpThreads() {
  int threads = 0;
#ifdef ICD9_OPENMP
  omp_sched_t sched;
  omp_get_schedule(&sched, &threads);
#endif
  return threads;
}

void debug_parallel() {
#if defined(ICD9_OPENMP) && defined(ICD9_DEBUG_PARALLEL)
  Rcpp::Rcout << "threads per omp_get_schedule = " << getOmpThreads() << ". ";
  Rcpp::Rcout << "max threads per omp_get_schedule = " << getOmpMaxThreads() << ". ";
  Rcpp::Rcout << "avail threads = " << omp_get_num_threads() << ". ";
  Rcpp::Rcout << "omp_get_thread_num = " << omp_get_thread_num() << ". ";
  Rcpp::Rcout << "omp_get_num_procs = " << getOmpCores() << "\n";
#else
  Rcpp::Rcout << "ICD9_OPENMP is not defined\n";
#endif
}

// [[Rcpp::export]]
Rcpp::NumericVector randomMajorCpp(int	n) {
  // This could just be a sprintf like the others.
  Rcpp::NumericVector iv = Rcpp::floor(Rcpp::runif(n) * 999);
  return iv;
}

//' @title generate random short-form numeric icd9 codes
//' @keywords internal
// [[Rcpp::export]]
std::vector<std::string> icd9RandomShortN(std::vector<std::string>::size_type n = 5) {
  VecStr out(n);
  std::vector<double> randoms = Rcpp::as<std::vector<double> >(Rcpp::runif(n, 0, 99999));
  char buffer[5];
  for (std::vector<double>::size_type i = 0; i != n; ++i) {
    sprintf(buffer, "%.0f", randoms[i]);
    out[i] = buffer;
  }
  return out;
}

//' @title generate random short-form icd9 V codes
//' @keywords internal
// [[Rcpp::export]]
std::vector<std::string> icd9RandomShortV(std::vector<std::string>::size_type n = 5) {
  VecStr out(n);
  std::vector<double> randoms = Rcpp::as<std::vector<double> >(Rcpp::runif(n, 0, 9999));
  char buffer[5];
  for (std::vector<double>::size_type i = 0; i != n; ++i) {
    sprintf(buffer, "V%.0f", randoms[i]);
    out[i] = buffer;
  }
  return out;
}

//' @title generate random short-form icd9 E codes
//' @keywords internal
// [[Rcpp::export]]
std::vector<std::string> icd9RandomShortE(std::vector<std::string>::size_type n = 5) {
  VecStr out(n);
  std::vector<double> randoms = Rcpp::as<std::vector<double> >(Rcpp::runif(n, 0, 9999));
  char buffer[5];
  for (std::vector<double>::size_type i = 0; i != n; ++i) {
    sprintf(buffer, "E%.0f", randoms[i]);
    out[i] = buffer;
  }
  return out;
}

//' @title generate random short-form icd9 E codes
//' @description Very dirty pseudorandom by picking numeric, V or E based on modulo 3 of the number
//' @keywords internal
// [[Rcpp::export]]
std::vector<std::string> icd9RandomShort(std::vector<std::string>::size_type n = 5) {
  VecStr out(n);
  std::vector<double> randoms = Rcpp::as<std::vector<double> >(Rcpp::runif(n, 0, 99999));
  char buffer[5];
  for (std::vector<double>::size_type i = 0; i != n; ++i) {
    // N, V or E?
    switch ((int)randoms[i] % 3) {
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
//' @rdname fastIntToString
//' @title fast convert integer vector to character vector
//' @param x td::vector<int>
//' @param bufferSize int if any input strings are longer than this number (default 16) there will be memory errors.
//'   No checks done for speed.
//' @examples
//' \dontrun{
//' pts <- randomPatients(1e7)
//' # conclusion: buffer size matters little (so default to be more generous), and Rcpp version fastest.
//' microbenchmark::microbenchmark(fastIntToStringStd(pts$visitId, buffer = 8),
//'                                fastIntToStringStd(pts$visitId, buffer = 16),
//'                                fastIntToStringStd(pts$visitId, buffer = 64),
//'                                fastIntToStringRcpp(pts$visitId, buffer = 8),
//'                                fastIntToStringRcpp(pts$visitId, buffer = 16),
//'                                fastIntToStringRcpp(pts$visitId, buffer = 64),
//'                                as.character(pts$visitId),
//'                                asCharacterNoWarn(pts$visitId), times = 5)
//' }
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
int callgrindStart(bool zerostats = false) {
#ifdef ICD9_VALGRIND
#ifdef ICD9_DEBUG
  Rcpp::Rcout << "Starting callgrind instrumentation...\n";
#endif
  CALLGRIND_START_INSTRUMENTATION;
  if (zerostats) {
#ifdef ICD9_DEBUG
    Rcpp::Rcout << "Zeroing callgrind stats.\n";
#endif
    CALLGRIND_ZERO_STATS;
  }
#endif
  return 0;
}

// [[Rcpp::export]]
int valgrindCallgrindStart(bool zerostats = false) {
#ifdef ICD9_VALGRIND
#ifdef ICD9_DEBUG
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
