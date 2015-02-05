// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>

#define ICD9_DEBUG = 1

typedef std::string Str;
typedef std::vector<Str> VecStr;
typedef std::vector<bool> VB;

typedef VecStr::iterator VecStrIt;

typedef std::set<Str> SetStr;
typedef std::map<Str,VecStr> MapVecStr;
typedef std::map<int,Str> MapStr;
typedef std::vector<SetStr> CmbMap;
typedef std::multimap<Str, Str> Tmm;
// internal function definitions
void printVecStr(VecStr sv);
void printCharVec(Rcpp::CharacterVector cv);
