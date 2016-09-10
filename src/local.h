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

#ifndef LOCAL_H_
#define LOCAL_H_

#include "config.h"
#include "icd_types.h"
#include <Rcpp.h>

extern "C" {
#include "cutil.h"
#include <cstdlib>
}

// these are feature requests: if not available they are disabled.
#define ICD_OPENMP
// #define ICD_STD_PARALLEL // don't use right now: see comment below

// debugging:
#define ICD_DEBUG
// #define ICD_DEBUG_TRACE
// #define ICD_DEBUG_SETUP
// #define ICD_DEBUG_SETUP_TRACE
// #define ICD_DEBUG_PARALLEL
#define ICD_VALGRIND

// enabling this stops the package compiling, but is useful for testing purely
// in C++. See tools/standalone.sh
// #define ICD_STANDALONE

// not enough to test whether header is available, because it may be disabled in
// R: #ifdef _OPENMP
#ifdef HAVE_R_OPENMP
#include <omp.h>
// openmp is required for GLIBC standard library parallel alternatives:
// now was parallel mode STL requested?
#ifdef ICD_STD_PARALLEL
// WORKING_PARALLEL_ALGORITHM is defined by configure script, but at present
// always disabled because it leads to ?false positive 'abort' and 'printf'
// found during R CMD check --as-cran
#ifndef WORKING_PARALLEL_ALGORITHM
// but not available, so disable
#undef ICD_STD_PARALLEL
#endif
#endif
#else
// OpenMP requested, but not available
#undef ICD_OPENMP
#endif

#ifdef ICD_VALGRIND
#ifdef HAVE_VALGRIND_VALGRIND_H
#include <valgrind/callgrind.h>
#else
#undef ICD_VALGRIND
#endif
#endif

#include <set>

// use flag set by configure
#ifdef HAVE_CXX11
#include <unordered_map>
#include <unordered_set>
typedef std::unordered_map<std::string, VecInt::size_type> VisLk;
typedef std::unordered_set<std::string> icd_set;
#else
typedef std::map<Str, VecInt::size_type> VisLk;
typedef std::set<Str> icd_set;
#endif

void buildMap(const Rcpp::List& icd9Mapping, VecVecInt& map);
void buildVisitCodesVec(const SEXP& icd9df, const std::string& visitId,
                        const std::string& icd9Field, VecVecInt& vcdb, VecStr& visitIds,
                        const bool aggregate);
ComorbidOut lookupComorbidByChunkFor(const VecVecInt& vcdb,
                                     const VecVecInt& map, const int chunkSize, const int ompChunkSize);

#if (defined ICD_DEBUG || defined ICD_DEBUG_SETUP)
#include <iostream>
// only include Rcpp::Rcout if debugging: R won't like cout so we should not do
// this unless debugging. not so easy to get an iterator for any std container
// (no common parent class), without Boost
template<typename VT>
void printIt(std::vector<VT> v) {
  typename std::vector<VT>::iterator i;
  std::ostringstream o;
  for (i=v.begin(); i!=v.end(); ++i) {o << *i << " ";}
  o << "\n";
  Rcpp::Rcout << o.str();
  Rcpp::Rcout.flush();
}

//overload for set
template<typename ST>
void printIt(std::set<ST> v) {
  typename std::set<ST>::iterator i;
  std::ostringstream o;
  for (i=v.begin(); i!=v.end(); ++i) {o << *i << " ";}
  o << "\n";
  Rcpp::Rcout << o.str();
  Rcpp::Rcout.flush();
}

//overload for map
template<typename MK, typename MV>
void printIt(std::map<MK,MV> v) {
  typename std::vector<MK,MV>::iterator i;
  std::ostringstream o;
  for (i=v.begin(); i!=v.end(); ++i) {o << *i << " ";}
  o << "\n";
  Rcpp::Rcout << o.str();
  Rcpp::Rcout.flush();
}

#endif // end (defined ICD_DEBUG || defined ICD_DEBUG_SETUP)

#endif // LOCAL_H_

