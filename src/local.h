// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::depends(RcppProgress)]]
// [[Rcpp::plugins(openmp)]]
#include <progress.hpp>
#include <Rcpp.h>

#define DEBUG = 1

typedef std::vector<std::string > VecStr;
typedef VecStr::iterator VecStrIt;
typedef std::set<std::string > SetStr;
typedef std::map<int,std::string > MapStr;
typedef std::vector<SetStr > CmbMap;
typedef std::multimap<std::string, std::string> Tmm;
// internal function definitions
int printVecStr(VecStr sv);
int printCharVec(CharacterVector cv);
