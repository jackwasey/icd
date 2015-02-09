// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <boost/container/flat_map.hpp> // may be better, but doesn't get parallelized by Rcpp
#include <boost/container/flat_set.hpp>

// #define ICD9_DEBUG = 1
// #define ICD9_TRACE = 1

typedef std::string Str;
typedef std::vector<Str> VecStr;
typedef std::vector<bool> VecBool;

typedef std::set<Str> SetStr;
typedef boost::container::flat_set<Str> BoostSetStr;
typedef std::map<Str, VecStr> MapVecStr;
typedef boost::container::flat_map<Str, VecStr> BoostMapVecStr; // turns out this is very slow, and doesn't parallelize
typedef std::vector< std::vector<std::string> > VecCodes;

typedef std::vector<SetStr> CmbMap; // ? faster with boost flat_sets: many lookups but each is from relatively small set
typedef std::vector<std::vector<std::string > > ComorbidVecMap;
typedef std::vector<BoostSetStr> BoostCmbMap; // ? faster with boost flat_sets: many lookups but each is from relatively small set

typedef std::multimap<Str, Str> MapVisitCode; // used in non-parallel implementation
typedef boost::container::flat_multimap<Str, Str> BoostMapVisitCode; // used in non-parallel implementation

// internal function definitions
void printVecStr(VecStr sv);
void printSetStr(SetStr vs);
void printCharVec(Rcpp::CharacterVector cv);
