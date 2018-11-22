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

#ifndef REFACTOR_H_
#define REFACTOR_H_

#include "local.h"
#include <Rcpp.h>

using namespace Rcpp;
IntegerVector factorNoSort(const CharacterVector& x,
                           const CharacterVector& levels,
                           const bool na_rm);
Rcpp::IntegerVector refactor(const IntegerVector& x,
                             const CV& new_levels, bool exclude_na);
Rcpp::IntegerVector refactor_narm(const IntegerVector& x, const CV& new_levels);
bool factorIsValid(const IntegerVector& f);
#endif /* REFACTOR_H_ */
