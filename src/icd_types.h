#ifndef ICD_TYPES_H_
#define ICD_TYPES_H_

// before including Eigen, if we are in debug mode, can be more aggressive
// about blocking spurious Eigen compilation warnings. These pragmas are not
// cross platform so cannot be in build done by CRAN
#ifndef EIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS
// before including Eigen
#define EIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS
#endif
#ifdef ICD_DEBUG
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Weverything"
//#pragma warning(push, 0)
#include <RcppEigen.h>
//#pragma warning(pop)
#pragma clang diagnostic pop
#endif

#include <Rcpp.h>
typedef Rcpp::CharacterVector CV;
#include <Eigen/Eigen>
#include <Eigen/Sparse>
#include <RcppEigen.h>

#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

typedef std::vector<std::string> VecStr;
typedef std::unordered_map<std::string, int> RelMap;

typedef std::unordered_set<std::string> US;
typedef std::string Str;
typedef std::vector<std::string> VecStr;

typedef Eigen::Triplet<int, R_xlen_t> Triplet;
typedef Eigen::SparseMatrix<int, Eigen::RowMajor> PtsSparse;
typedef Eigen::MatrixXi DenseMap;

#endif /* ICD_TYPES_H_ */
