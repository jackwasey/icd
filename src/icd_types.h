#ifndef ICD_TYPES_H_
#define ICD_TYPES_H_

// also add LinkingTo element in DESCRIPTION to enable
#include <RcppEigen.h>
#include <Eigen/SparseCore>

#include <vector>
#include <string>
#include <unordered_map>
#include <unordered_set>
typedef std::string Str;
typedef std::vector<Str> VecStr;
typedef std::vector<int> VecInt;
typedef std::vector<bool> VecBool; // naughty, but good compromise
typedef std::vector<char> VecChar;
typedef VecChar::iterator VecCharIt;
typedef VecInt::const_iterator VecIntIt;
// SOMEDAY: replace int with char, but this stops Rcpp::export working
typedef VecInt ComorbidOut;
typedef std::vector<VecStr> VecVecStr;
typedef std::vector<VecInt> VecVecInt;
typedef std::vector<VecBool> VecVecBool;
typedef VecVecInt::size_type VecVecIntSz;
typedef VecInt NewOutPt;
typedef std::vector<NewOutPt> NewOut;
typedef VecVecInt::iterator NewOutIt;
typedef std::unordered_map<std::string, VecInt::size_type> VisLk;
typedef std::unordered_map<std::string, int> RelMap;
typedef std::pair<std::string, int> RelPair;
typedef std::unordered_set<std::string> icd_set;
typedef std::unordered_set<std::string> US;
typedef Rcpp::CharacterVector CV;

typedef int SparseValue;
typedef Eigen::Triplet<SparseValue> Triplet;
typedef Eigen::SparseMatrix<SparseValue, Eigen::RowMajor> PtsSparse;
typedef Eigen::MatrixXi DenseMap;
typedef std::pair<std::string, VecInt::size_type> VisLkPair;
typedef Rcpp::sugar::IndexHash<STRSXP> IHS;

#endif /* ICD_TYPES_H_ */
