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

#ifndef RANGES_ALT_H_
#define RANGES_ALT_H_

#include "icd_types.h"
#include <Rcpp.h>

#include "range-const.h"
#include <stddef.h> // for size_t

// define end() to get end index of a C array
template<typename T, size_t N>
T * end(T (&ra)[N]) {
  return ra + N;
}

const VecStr vv_std(vv_char, end(vv_char));
const VecStr v0_std(v0_char, end(v0_char));
const VecStr v1_std(v1_char, end(v1_char));
const VecStr v2_std(v2_char, end(v2_char));
const VecStr v3_std(v3_char, end(v3_char));
const VecStr v4_std(v4_char, end(v4_char));
const VecStr v5_std(v5_char, end(v5_char));
const VecStr v6_std(v6_char, end(v6_char));
const VecStr v7_std(v7_char, end(v7_char));
const VecStr v8_std(v8_char, end(v8_char));
const VecStr v9_std(v9_char, end(v9_char));
const VecStr vbase_e_std(vbase_e_char, end(vbase_e_char));
const VecStr v_empty_std(0L);

VecStr icd9ExpandMinor_alt_Std(const Str& mnr, bool isE);
CV icd9ChildrenShort_alt_11(CV icd9Short, bool onlyReal);

#endif /* RANGES_ALT_H_ */
