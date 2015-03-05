// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include <R.h>
#include <Rinternals.h>
#include <string>
#include <algorithm>
#include <vector>
#include <set>

extern "C" {
#include "local_c.h"
#include <cstdlib>
}

//#define ICD9_DEBUG
//#define ICD9_DEBUG_TRACE
//#define ICD9_DEBUG_SETUP
//#define ICD9_DEBUG_SETUP_TRACE
//#define ICD9_DEBUG_PARALLEL
//#define ICD9_VALGRIND
#ifdef _OPENMP
#include <omp.h>
#define ICD9_OPENMP
#define ICD9_ORDER_GUARANTEE
#endif

#ifdef _OPENMP // not available on clang
#endif
// enable linux performance counting
#ifdef ICD9_VALGRIND
#include <valgrind/callgrind.h>
#endif

typedef std::string Str;
typedef std::vector<Str> VecStr;

typedef std::vector<int> VecInt;
typedef std::vector<char> ComorbidOut; // TODO: someday benchmark int vs char (or possibly Boost bitset)
// vector<bool> dangerous with multiple threads, and note that char doesn't cast to bool with Rcpp

typedef std::vector<VecStr> VecVecStr;
typedef std::vector<VecInt> VecVecInt;
typedef VecVecInt::size_type VecVecIntSz;

void buildMap(const Rcpp::List& icd9Mapping, VecVecInt& map);
void buildVisitCodesVec(const SEXP& icd9df, const std::string& visitId,
		const std::string& icd9Field, VecVecInt& vcdb, VecStr& visitIds,
		const bool aggregate);

#if (defined ICD9_DEBUG || defined ICD9_DEBUG_SETUP)
#include <iostream> // only include std::cout if debugging: R won't like cout so we should not do this unless debugging.
// not so easy to get an iterator for any std container (no common parent class), without Boost
template<typename VT>
void printIt(std::vector<VT> v) {
	typename std::vector<VT>::iterator i;
	std::ostringstream o;
	for (i=v.begin(); i!=v.end(); ++i) {o << *i << " ";}
	o << "\n";
	std::cout << o.str();
	std::cout.flush();
}

//template<typename T>
//void printIt(boost::unordered_set<T> v) {
//	typename boost::unordered_set<T>::iterator i;
//	std::ostringstream o;
//	for (i=v.begin(); i!=v.end(); ++i) { o << *i << " "; }
//	o << "\n";
//	std::cout << o.str();
//	std::cout.flush();
//}

//overload for set
template<typename ST>
void printIt(std::set<ST> v) {
	typename std::set<ST>::iterator i;
	std::ostringstream o;
	for (i=v.begin(); i!=v.end(); ++i) {o << *i << " ";}
	o << "\n";
	std::cout << o.str();
	std::cout.flush();
}

//overload for map
template<typename MK, typename MV>
void printIt(std::map<MK,MV> v) {
	typename std::vector<MK,MV>::iterator i;
	std::ostringstream o;
	for (i=v.begin(); i!=v.end(); ++i) {o << *i << " ";}
	o << "\n";
	std::cout << o.str();
	std::cout.flush();
}

void printCharVec(Rcpp::CharacterVector cv);
#endif

