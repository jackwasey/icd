// Copyright (C) 2014 - 2016  Jack O. Wasey
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

#include <vector>
#include <string>
typedef std::string Str;
typedef std::vector<Str> VecStr;

typedef std::vector<int> VecInt;

// SOMEDAY replace with char, but this stops Rcpp::export working
typedef std::vector<int> ComorbidOut;

typedef std::vector<VecStr> VecVecStr;
typedef std::vector<VecInt> VecVecInt;
typedef VecVecInt::size_type VecVecIntSz;

#endif
