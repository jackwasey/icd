// Copyright (C) 2014 - 2018  Jack O. Wasey
//
// This file is part of icd.
//
// icd is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// icd is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with icd. If not, see <http://www.gnu.org/licenses/>.

#ifndef ICD_TYPES_H_
#define ICD_TYPES_H_

#include <Rcpp.h>
#include "config.h" // to know whether we have OpenMP, Eigen, etc. which have typedefs
#include <vector>
#include <string>
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
typedef std::unordered_set<std::string> icd_set;
typedef Rcpp::CharacterVector CV;

#include <RcppEigen.h> // also add LinkingTo element in DESCRIPTION to enable
#include <Eigen/SparseCore>

// using the typedef confuses Rcpp?
//typedef Eigen::SparseMatrix<char, Eigen::RowMajor> SparseOut; // bool, char or int?
// https://eigen.tuxfamily.org/dox/group__TutorialSparse.html
typedef int SparseValue;
typedef Eigen::Triplet<SparseValue> Triplet;
typedef Eigen::SparseMatrix<SparseValue, Eigen::RowMajor> PtsSparse;
typedef Eigen::MatrixXi DenseMap; // col major unless otherwise stated, I think

typedef std::pair<std::string, VecInt::size_type> VisLkPair;

typedef Rcpp::sugar::IndexHash<STRSXP> IHS;

#endif /* ICD_TYPES_H_ */
