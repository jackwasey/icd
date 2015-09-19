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
#endif
//#include <Rinternals.h>

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
std::string strimCpp(std::string s) { // according to http://stackoverflow.com/questions/10789740/passing-stdstring-by-value-or-reference
// C++11 (i.e. almost everyone) will avoid copy even without using reference argument.
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
	return true; // Rcpp fails with void for some reason
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
#endif
}
// header for this is in local.h because it doesn't get picked up in util.h for some reason


// [[Rcpp::export]]
Rcpp::NumericVector randomMajorCpp(int	n) {
	Rcpp::NumericVector iv = Rcpp::floor(Rcpp::runif(n) * 999);
	return iv;
}

//' genereate random short icd9 codes
//' @keywords internal
//' @importFrom stats runif
// [[Rcpp::export]]
std::vector<std::string> randomShortIcd9(std::vector<std::string>::size_type n = 50000) {
	static const char alphanum[] = "0123456789";

	VecStr out(n);

  //as.character(floor(stats::runif(min = 1, max = 99999, n = n)))
		  return out;
}

  void gen_random(char *s, const int len) {
    static const char alphanum[] =
      "0123456789"
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      "abcdefghijklmnopqrstuvwxyz";

    for (int i = 0; i < len; ++i) {
      s[i] = alphanum[rand() % (sizeof(alphanum) - 1)];
    }

    s[len] = 0;
  }
