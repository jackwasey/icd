// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include <string>
#include <vector>
#include <set>
//#include <boost/unordered/unordered_set.hpp>

//#define ICD9_DEBUG
#define ICD9_DEBUG_SETUP
//#define ICD9_DEBUG_SETUP_TRACE
//#define ICD9_TRACE
//#define ICD9_DEBUG_PARALLEL
//#define ICD9_VALGRIND
#ifdef _OPENMP
//#define ICD9_OPENMP
#define ICD9_ORDER_GUARANTEE
#endif

#ifdef ICD9_VALGRIND
#include <valgrind/callgrind.h>
#endif

typedef std::string Str;
typedef std::vector<Str> VecStr;

//typedef std::vector<bool> VecBool;
typedef std::vector<int> VecInt;
//typedef std::vector<unsigned int> VecUInt; // doesn't work well with Rcpp
typedef VecInt Out; // TODO: would rather use char or bool, or something more compact, but vector<bool> dangerous with multiple threads.
typedef VecInt Codes; // e.g. the codes in a comorbidity subtype (ie numeric, V, or E)
//Boost has a safer option. Anyway, R itself tends to be faster with ints than bools. Doubt there are big optimizations here.

typedef std::set<Str> SetStr;
typedef std::set<int> SetInt;
typedef std::set<unsigned int> SetUInt;

typedef std::vector< VecStr > VecCodes; // obsolete, may be needed for pre-processing
typedef std::vector< VecInt > CodesVecSubtype;
//typedef CodesVecSubtype::size_type Visit_size_t;

typedef std::vector<VecStr> ComorbidVecMap;
typedef std::vector<VecInt> ComorbidVecInt;

//typedef std::multimap<Str, Str> MapVisitCode; // used in non-parallel implementation
typedef std::map<std::string,VecStr> MMVisitCodes;
//typedef boost::unordered_set<std::string> UnSet;

// internal function definitions
#if (defined ICD9_DEBUG || defined ICD9_DEBUG_SETUP)
#include <iostream> // only include std::cout if debugging: R won't like cout so we should not do this unless debugging.
// not so easy to get an iterator for any std container (no common parent class), without Boost
template<typename VT>
void printIt(std::vector<VT> v) {
	typename std::vector<VT>::iterator i;
	std::ostringstream o;
	for (i=v.begin(); i!=v.end(); ++i) { o << *i << " "; }
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
	for (i=v.begin(); i!=v.end(); ++i) { o << *i << " "; }
	o << "\n";
	std::cout << o.str();
	std::cout.flush();
}

//overload for map
template<typename MK, typename MV>
void printIt(std::map<MK,MV> v) {
	typename std::vector<MK,MV>::iterator i;
	std::ostringstream o;
	for (i=v.begin(); i!=v.end(); ++i) { o << *i << " "; }
	o << "\n";
	std::cout << o.str();
	std::cout.flush();
}
#endif
void printCharVec(Rcpp::CharacterVector cv);

void buildMap(const Rcpp::List& icd9Mapping, ComorbidVecInt& map_n, ComorbidVecInt& map_v, ComorbidVecInt& map_e);

void buildVisitCodesVec(const Rcpp::DataFrame& icd9df, const std::string& visitId, const std::string& icd9Field,
		CodesVecSubtype& vcdb_n, CodesVecSubtype& vcdb_v, CodesVecSubtype& vcdb_e, VecStr& visitIds);

Out lookupComorbidByChunkFor(const CodesVecSubtype& vcdb_n, const CodesVecSubtype& vcdb_v, const CodesVecSubtype& vcdb_e,
		const ComorbidVecInt& map_n, const ComorbidVecInt& map_v, const ComorbidVecInt& map_e,
		const int chunkSize, const int ompChunkSize);
