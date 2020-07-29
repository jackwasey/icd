#ifndef ICD_TYPES_H_
#define ICD_TYPES_H_

// also add LinkingTo element in DESCRIPTION to enable
#include <RcppEigen.h>
#include <Eigen/SparseCore>

#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>
typedef std::vector<std::string> VecStr;
typedef std::unordered_map<std::string, std::vector<int>::size_type> VisLk;
typedef std::unordered_map<std::string, int> RelMap;
typedef std::unordered_set<std::string> icd_set;
typedef std::unordered_set<std::string> US;
typedef Rcpp::CharacterVector CV;

typedef Eigen::Triplet<int> Triplet;
typedef Eigen::SparseMatrix<int, Eigen::RowMajor> PtsSparse;
typedef Eigen::MatrixXi DenseMap;
typedef std::pair<std::string, std::vector<int>::size_type> VisLkPair;
typedef Rcpp::sugar::IndexHash<STRSXP> IHS;

#endif /* ICD_TYPES_H_ */
