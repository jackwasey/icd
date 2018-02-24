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

#ifndef MANIP_ALT_H_
#define MANIP_ALT_H_

// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include "Rcpp/String.h"  // for String
#include "icd_types.h"    // for CV

Rcpp::String icd9AddLeadingZeroesShortSingle(Rcpp::String x);
CV icd9AddLeadingZeroesDirect(CV x, bool short_code);

#endif /* MANIP_ALT_H_ */
