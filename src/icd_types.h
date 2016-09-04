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

#ifndef TYPES_H_
#define TYPES_H_

#include <vector>
typedef std::string Str;
typedef std::vector<Str> VecStr;

typedef std::vector<int> VecInt;
typedef std::vector<char> ComorbidOut;
// this could be int ( which works around Rcpp bug which can't cast vector<char>
// to LogicalVector). Will need to benchmark. vector<bool> dangerous with
// multiple threads

typedef std::vector<VecStr> VecVecStr;
typedef std::vector<VecInt> VecVecInt;
typedef VecVecInt::size_type VecVecIntSz;

#endif
