// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <string>
#include <vector>
#include <map>
#include <set>

#ifdef ICD9_BOOST
#include <boost/container/flat_map.hpp> // may be better, but doesn't get parallelized by Rcpp
#include <boost/container/flat_set.hpp>
// try with boost (much slower and doesn't thread, at least in my first implementation)
typedef boost::container::flat_set<Str> BoostSetStr;
typedef boost::container::flat_map<Str, VecStr> BoostMapVecStr; // turns out this is very slow, and doesn't parallelize
typedef boost::container::flat_multimap<Str, Str> BoostMapVisitCode; // used in non-parallel implementation
typedef std::vector<BoostSetStr> BoostCmbMap; // ? faster with boost flat_sets: many lookups but each is from relatively small set
#endif

#define ICD9_DEBUG
//#define ICD9_DEBUG_SETUP
//#define ICD9_TRACE
//#define ICD9_DEBUG_PARALLEL

typedef std::string Str;
typedef std::vector<Str> VecStr;
typedef std::vector<bool> VecBool;
typedef std::vector<int> VecInt;
typedef std::vector<unsigned int> VecUInt;

typedef std::set<Str> SetStr;
typedef std::set<int> SetInt;
typedef std::set<unsigned int> SetUInt;

typedef std::map<Str, VecStr> MapVecStr;
typedef std::map<Str, VecUInt> MapVecInt;
typedef std::vector< VecStr > VecCodes;
typedef std::vector< VecInt > VecIntCodes;

typedef std::vector<SetStr> CmbMap;
typedef std::vector<VecStr> ComorbidVecMap;
typedef std::vector<VecUInt> ComorbidVecInt;
typedef std::vector<SetUInt> ComorbidSetInt;

typedef std::multimap<Str, Str> MapVisitCode; // used in non-parallel implementation

// internal function definitions
#ifdef ICD9_DEBUG
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
	return;
}

//overload for set
template<typename ST>
void printIt(std::set<ST> v) {
	typename std::set<ST>::iterator i;
	std::ostringstream o;
	for (i=v.begin(); i!=v.end(); ++i) { o << *i << " "; }
	o << "\n";
	std::cout << o.str();
	std::cout.flush();
	return;
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
	return;
}
#endif
void printCharVec(Rcpp::CharacterVector cv);

