// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>

#define ICD9_DEBUG = 1

typedef std::vector<std::string > VecStr;
typedef std::vector<bool> VB;
typedef VecStr::iterator VecStrIt;
typedef std::set<std::string > SetStr;
typedef std::map<int,std::string > MapStr;
typedef std::vector<SetStr > CmbMap;
typedef std::multimap<std::string, std::string> Tmm;
// internal function definitions
int printVecStr(VecStr sv);
int printCharVec(Rcpp::CharacterVector cv);
