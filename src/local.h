/*
 * local.h
 *
 *  Created on: Mar 7, 2015
 *      Author: jack
 */

#ifndef LOCAL_H_
#define LOCAL_H_

// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
//#include <R.h>
//#include <Rinternals.h>
//#include <string>
//#include <algorithm>
#include <vector>
#include <set>

extern "C" {
#include "cutil.h"
#include <cstdlib>
}

#define ICD9_DEBUG
//#define ICD9_DEBUG_TRACE
//#define ICD9_DEBUG_SETUP
//#define ICD9_DEBUG_SETUP_TRACE
//#define ICD9_DEBUG_PARALLEL
//#define ICD9_VALGRIND

#ifdef _OPENMP
#include <omp.h>
#define ICD9_OPENMP
#endif

#ifdef ICD9_VALGRIND
#include <valgrind/callgrind.h>
#endif

typedef std::string Str;
typedef std::vector<Str> VecStr;

typedef std::vector<int> VecInt;
typedef std::vector<char> ComorbidOut; // TODO: someday benchmark int vs char (or possibly Boost bitset)
// vector<bool> dangerous with multiple threads, and note that char doesn't cast to bool (directly) with Rcpp, yet.

typedef std::vector<VecStr> VecVecStr;
typedef std::vector<VecInt> VecVecInt;
typedef VecVecInt::size_type VecVecIntSz;

#ifdef _GLIBCXX_UNORDERED_MAP
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

void printCharVec(Rcpp::CharacterVector cv);
#endif

#endif // LOCAL_H_
