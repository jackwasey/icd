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
// [[Rcpp::plugins(openmp)]]
#include "local.h"                             // for ICD_OPENMP
#include "config.h"                             // for ICD_VALGRIND
#include "util.h"
#include <stdlib.h>
#include <math.h>                              // for floor
#include <stdio.h>                             // for sprintf
#include <string.h>                            // for strcmp
#include <algorithm>                           // for copy, sort, transform
#include <iterator>                            // for back_insert_iterator
#include <ostream>                             // for size_t, operator<<
#include <string>                              // for string, basic_string
#include <vector>                              // for vector, vector<>::size...
#ifdef ICD_OPENMP
#include <omp.h>
#endif

using Rcpp::List;
using Rcpp::Vector;
using Rcpp::LogicalVector;
using Rcpp::IntegerVector;
using Rcpp::CharacterVector;
using Rcpp::String;
using Rcpp::Rcout;
using Rcpp::as;
using Rcpp::Named;

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
VecStr trimCpp(VecStr sv) {
  for (VecStr::iterator i = sv.begin(); i != sv.end(); ++i)
    *i = strimCpp(*i);
  return sv;
}

// [[Rcpp::export(get_omp_cores)]]
int getOmpCores() {
  int cores = 0;
#ifdef ICD_OPENMP
  cores = omp_get_num_procs();
#endif
  return cores;
}

// nocov start

// [[Rcpp::export(get_omp_max_threads)]]
int getOmpMaxThreads() {
  int maxthreads = 0;
#ifdef ICD_OPENMP
  maxthreads = omp_get_max_threads();
#endif
  return maxthreads;
}

// https://stackoverflow.com/questions/43736622/which-openmp-schedule-am-i-running/43755259#43755259
// [[Rcpp::export(get_omp_threads)]]
int getOmpThreads() {
  int threads = 0;
#ifdef ICD_OPENMP
  omp_sched_t sched;
  omp_get_schedule(&sched, &threads);
#endif
  return threads;
}

// [[Rcpp::export]]
void debug_parallel_env() {
#ifdef ICD_DEBUG_PARALLEL
  Rcout << "checking OpenMP flags...\n";
#ifdef HAVE_R_OPENMP
  Rcout << "HAVE_R_OPENMP is defined.\n";
#endif
#ifdef _OPENMP
  Rcout << "_OPENMP is defined.\n";
#else
  Rcout << "_OPENMP is not defined.\n";
#endif

#ifdef ICD_OPENMP
  Rcout << "ICD_OPENMP is defined.\n";
#else
  Rcout << "ICD_OPENMP is not defined.\n";
#endif
#endif
}

// [[Rcpp::export]]
void debug_parallel() {
  //  cannot use Rcpp:Rcout in multithreaded code: alternative (for debugging
  //  only) is RcppThreads. Small package but I'm reluctant to add another
  //  dependency.
  /*
#if defined(ICD_OPENMP) && defined(ICD_DEBUG_PARALLEL)
   Rcpp::Rcout << "threads per omp_get_schedule = " << getOmpThreads()
               << " max threads per omp_get_schedule = " << getOmpMaxThreads()
               << " avail threads = " << omp_get_num_threads()
               << " omp_get_thread_num = " << omp_get_thread_num()
               << " omp_get_num_procs = " << getOmpCores() << "\n";
#endif // ICD_DEBUG_PARALLEL
   */
}

// nocov end

// [[Rcpp::export]]
Rcpp::NumericVector randomMajorCpp(int	n) {
  // This could just be a sprintf like the others.
  Rcpp::NumericVector iv = Rcpp::floor(Rcpp::runif(n) * 999);
  return iv;
}

//' @rdname icd9RandomShort
//' @keywords internal
// [[Rcpp::export]]
VecStr icd9RandomShortN(VecStr::size_type n = 5) {
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
VecStr icd9RandomShortV(VecStr::size_type n = 5) {
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
VecStr icd9RandomShortE(VecStr::size_type n = 5) {
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
VecStr icd9RandomShort(VecStr::size_type n = 5) {

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

// [[Rcpp::export]]
int valgrindCallgrindStart(bool zerostats = false) {
  if (zerostats) {}; // no-op
#ifdef ICD_VALGRIND
  Rcout << "Starting callgrind instrumentation..." << std::endl;
  CALLGRIND_START_INSTRUMENTATION;
  if (zerostats) {
    Rcout << "Zeroing callgrind stats." << std::endl;
    CALLGRIND_ZERO_STATS;
  }
#else
  DEBUG("NOT starting Valgrind callgrind instrumentation, not linked");
#endif
  return 0;
}

// [[Rcpp::export]]
int valgrindCallgrindStop() {
#ifdef ICD_VALGRIND
  Rcout << "Stopping Valgrind callgrind instrumentation..." << std::endl;
  CALLGRIND_STOP_INSTRUMENTATION;
#else
  DEBUG("NOT stopping Valgrind callgrind instrumentation, not linked.");
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
  return icd9CompareStrings(af, bf);
}

// [[Rcpp::export(icd9_sort_cpp)]]
VecStr icd9SortCpp(VecStr x) {
  std::sort(x.begin(), x.end(), icd9CompareStrings);
  return x;
}

// add one because R indexes from 1, not 0
inline std::size_t getSecondPlusOne(const std::pair<std::string, std::size_t>& p) {
  return p.second + 1;
}

// [[Rcpp::export(icd9_order_cpp)]]
std::vector<std::size_t> icd9OrderCpp(VecStr x) {
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

//' @describeIn factor_nosort Rcpp implementation, requiring character vector
//'   inputs only, no argument checking.
//' @keywords internal manip
// [[Rcpp::export(factor_nosort_rcpp_worker)]]
IntegerVector factorNoSort(const Vector<STRSXP>& x,
                           const Vector<STRSXP>& levels) {
  IntegerVector out = match(x, levels);
  out.attr("levels") = as<CharacterVector>(levels);
  out.attr("class") = "factor";
  return out;
}

template <int RTYPE>
IntegerVector matchFastTemplate(const Vector<RTYPE>& x, const Vector<RTYPE>& table) {
  return(match(x, table));
}

//' @title Faster match
//' @name match_rcpp
//' @description Try Rcpp hashing (and simpler logic) compared to R's internal
//'   do_match and match5 morass. Lose the ability to use \code{incomparables}
//'   in initial implementation.
//' @keywords internal
// [[Rcpp::export(match_rcpp)]]
SEXP matchFast(SEXP x, SEXP table) {
  switch( TYPEOF(x) ) {
  case INTSXP: return matchFastTemplate<INTSXP>(x, table);
  case REALSXP: return matchFastTemplate<REALSXP>(x, table);
  case STRSXP: return matchFastTemplate<STRSXP>(x, table);
  }
  return R_NilValue;
}

template <int RTYPE>
LogicalVector inFastTemplate(const Vector<RTYPE>& x, const Vector<RTYPE>& table) {
  return(!is_na(match(x, table)));
}

//' @describeIn match_rcpp Use faster Rcpp matching for %in%
//' @keywords internal
// [[Rcpp::export(fin)]]
SEXP inFast(SEXP x, SEXP table) {
  switch( TYPEOF(x) ) {
  case INTSXP: return inFastTemplate<INTSXP>(x, table);
  case REALSXP: return inFastTemplate<REALSXP>(x, table);
  case STRSXP: return inFastTemplate<STRSXP>(x, table);
  }
  return R_NilValue;
}

//' @title Split a factor into two based on desired levels
//' @description Using C++ because I also want to quickly return the visits
//'   corresponding to the NA or non-NA factor elements.
//' @examples
//'   v <- c("X", "Y")
//'   df <- data.frame(visit_id = factor(c("visit1", "visit2", "visit1")),
//'                    icd_code = factor(c("410", "0010", "E999")))
//'   icd:::factor_split_rcpp(df, "410", "visit_id", "icd_code")
//' @keywords internal manip
// [[Rcpp::export(factor_split_rcpp)]]
List factorSplit(const List &df,
                 const CharacterVector &relevant,
                 const String &visit_name,
                 const String &code_name) {
  List out = List();
  // need to template over this for character or integer/factor?
  const CharacterVector visits = df[visit_name];
  const IntegerVector &x = df[code_name];
  VecStr no_na_lx_std;
  VecInt f_std;
  f_std.reserve(x.size());
  LogicalVector inc_mask(x.size());
  CharacterVector lx = x.attr("levels");
  no_na_lx_std.reserve(lx.size());
  DEBUG_VEC(x);
  DEBUG_VEC(lx);
  if (lx.isNULL()) Rcpp::stop("icd codes must be in a factor");
  for (auto l : lx) {
    if (l == NA_STRING) { // can't use is_na on a String
      DEBUG("Found NA in factor level");
      continue;
    }
    no_na_lx_std.push_back(as<std::string>(l));
  }
  DEBUG_VEC(no_na_lx_std);
  CV no_na_lx = Rcpp::wrap(no_na_lx_std);
  DEBUG_VEC(no_na_lx);
  DEBUG_VEC(relevant);
  IntegerVector new_level_idx = Rcpp::match(no_na_lx, relevant);
  DEBUG_VEC(new_level_idx);
  R_xlen_t fsz = x.size();
  DEBUG("fsz = " << fsz);
//#pragma omp parallel for
  for (R_xlen_t i = 0; i < fsz; ++i) {
    TRACE("considering index x[i] - 1: " << x[i] - 1 << " from new_level_idx");
    if (IntegerVector::is_na(x[i])) {
      inc_mask[i] = false;
      TRACE("inserting NA at pos " << i << "due to NA factor level in codes");
      continue;
    }
    auto cur = new_level_idx[x[i] - 1]; // get new index from old using lookup.
    if (IntegerVector::is_na(cur)) {
      inc_mask[i] = false;
      TRACE("inserting NA at pos " << i);
    } else {
      inc_mask[i] = true;
      f_std.push_back(cur);
      TRACE("inserting " << cur << " at pos " << i);
      assert(cur > 0);
    }
  }
  DEBUG_VEC(x);
  DEBUG_VEC(f_std);
  IntegerVector f = Rcpp::wrap(f_std);
  f.attr("levels") = relevant;
  f.attr("class") = "factor";
  CharacterVector all_visits_comorbid = visits[inc_mask];
  CharacterVector all_visits_no_comorbid =
    visits[is_na(match(visits, all_visits_comorbid))];
  assert((all(f > 0)).is_true()); // Rcpp::stop("index errors in code name");
  assert(max(f) < f.size() + 1); // if relevant was correct, this should be =
  //assert(max(f) == f.size() + 1);
  List comorbid_df = List::create(Named(visit_name) = all_visits_comorbid,
                                  Named(code_name) = f);
  Rcpp::CharacterVector rownames(f.size());
  char buffer[32];
  for (R_xlen_t i = 0; i != f.size(); ++i) {
    sprintf(buffer, "%lu", i);
    rownames(i) = buffer;
  }
  comorbid_df.attr("row.names") = rownames;
  comorbid_df.attr("names") = CharacterVector::create(visit_name, code_name);
  comorbid_df.attr("class") = "data.frame";
  out = List::create(
    Named("comorbid_df") = comorbid_df,
    Named("unique_no_comorbid") = unique(all_visits_no_comorbid));
  return out;
}
