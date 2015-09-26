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

#ifndef LOCAL_H_
#define LOCAL_H_

#include "config.h"

#include <Rcpp.h>
#include <vector>
#include <set>

extern "C" {
#include "cutil.h"
#include <cstdlib>
}

//#define ICD9_DEBUG
//#define ICD9_DEBUG_TRACE
//#define ICD9_DEBUG_SETUP
//#define ICD9_DEBUG_SETUP_TRACE
//#define ICD9_DEBUG_PARALLEL
#define ICD9_VALGRIND

// enabling this stops the package compiling, but is useful for testing purely
// in C++. See tools/standalone.sh
//#define ICD9_STANDALONE

// not enough to test whether header is available, because it may be disabled in R: #ifdef _OPENMP
#ifdef HAVE_R_OPENMP
#include <omp.h>
#define ICD9_OPENMP
// openmp is required for GLIBC standard library parallel alternatives
#ifdef HAVE_PARALLEL_PARALLEL_H
#define ICD9_STD_PARALLEL
#endif
#endif

#ifdef ICD9_VALGRIND
#ifdef HAVE_VALGRIND_VALGRIND_H
//__CALLGRIND_H
#include <valgrind/callgrind.h>
#else
#undef ICD9_VALGRIND
#endif
#endif

typedef std::string Str;
typedef std::vector<Str> VecStr;

typedef std::vector<int> VecInt;
typedef std::vector<char> ComorbidOut;
// this could be int ( which works around Rcpp bug which can't cast vector<char>
// to LogicalVector). Will need to benchmark. vector<bool> dangerous with
// multiple threads

typedef std::vector<VecStr> VecVecStr;
typedef std::vector<VecInt> VecVecInt;
typedef VecVecInt::size_type VecVecIntSz;

// use flag set by configure
#ifdef HAVE_CXX11
#include <unordered_map>
typedef std::unordered_map<std::string, VecInt::size_type> VisLk;
#else
typedef std::map<std::string, VecInt::size_type> VisLk;
#endif

void buildMap(const Rcpp::List& icd9Mapping, VecVecInt& map);
void buildVisitCodesVec(const SEXP& icd9df, const std::string& visitId,
		const std::string& icd9Field, VecVecInt& vcdb, VecStr& visitIds,
		const bool aggregate);
ComorbidOut lookupComorbidByChunkFor(const VecVecInt& vcdb,
		const VecVecInt& map, const int chunkSize, const int ompChunkSize);

#if (defined ICD9_DEBUG || defined ICD9_DEBUG_SETUP)
#include <iostream> // only include Rcpp::Rcout if debugging: R won't like cout so we should not do this unless debugging.
// not so easy to get an iterator for any std container (no common parent class), without Boost
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

#endif // end (defined ICD9_DEBUG || defined ICD9_DEBUG_SETUP)

#endif // LOCAL_H_

