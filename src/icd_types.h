#ifndef ICD_TYPES_H_
#define ICD_TYPES_H_

// Rcpp includes this first, so we can insert pragmas to stop RcppEigen (in
// particular) spewing out false-positive warnings.

// before including Eigen, if we are in debug mode, can be more aggressive
// about blocking spurious Eigen compilation warnings. These pragmas are not
// cross platform so cannot be in build done by CRAN
#ifndef EIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS
// before including Eigen
#define EIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS
#endif

// CRAN does not like limiting warnings, so only limit RcppEigen warnings on
// debug build so I can at least see warnings from my code.

#ifdef ICD_DEBUG
#if defined(__GNUC__) && !defined(__clang__)
#pragma warning(push, 0)
#elif defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Weverything"
#endif
#endif

#include <RcppEigen.h>

#ifdef ICD_DEBUG
#if defined(__GNUC__) && !defined(__clang__)
#pragma warning(pop)
#elif defined(__clang__)
#pragma clang diagnostic pop
#endif
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

// see Relevant class
#ifndef PTR_SET
typedef std::string US_T;
#else
// R may not use this type for its own pointers due to long/long long drama. Get
// type from R definitions?
typedef uintptr_t US_T;
#endif

typedef std::vector<US_T> IcdVec;
typedef std::unordered_set<US_T> IcdSet;
typedef std::string Str;
typedef std::vector<std::string> VecStr;
typedef std::unordered_map<US_T, int> IcdRelMap;

typedef Eigen::Triplet<int, R_xlen_t> Triplet;
typedef Eigen::SparseMatrix<int, Eigen::RowMajor> PtsSparse;
typedef Eigen::MatrixXi DenseMap;

#endif /* ICD_TYPES_H_ */
