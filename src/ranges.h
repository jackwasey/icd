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

#ifndef RANGES_H_
#define RANGES_H_

#include <stddef.h>     // for size_t
#include "icd_types.h"
#include <Rcpp.h>
#include "range-const.h"                    // for v_empty_std, v0, v0_std, v1

CV icd9ChildrenShort(CV icd9Short, const VecStr& icd9cmReal, bool onlyReal = true);
CV icd9ChildrenShortUnordered(CV icd9Short, const VecStr& icd9cmReal, bool onlyReal = true);
CV icd9ChildrenDecimalCpp(CV icd9Decimal, const VecStr& icd9cmReal, bool onlyReal = true);
CV icd9ExpandMinor(const Str& mnr, bool isE = false);

#endif /* RANGES_H_ */
