#ifndef ICD_TYPES_H_
#define ICD_TYPES_H_

// also add LinkingTo element in DESCRIPTION to enable
#include <Eigen/Sparse>
#include <RcppEigen.h>

#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

typedef std::vector<std::string> VecStr;
typedef std::unordered_map<std::string, int> RelMap;

typedef std::unordered_set<std::string> US;
typedef Rcpp::CharacterVector CV;
typedef std::string Str;
typedef std::vector<std::string> VecStr;

typedef Eigen::Triplet<int> Triplet;
typedef Eigen::SparseMatrix<int, Eigen::RowMajor> PtsSparse;
typedef Eigen::MatrixXi DenseMap;
typedef Rcpp::sugar::IndexHash<STRSXP> IHS;

#endif /* ICD_TYPES_H_ */
