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

#ifndef MANIP_H_
#define MANIP_H_

// [[Rcpp::interfaces(r, cpp)]]
#include "icd_types.h"
#include "Rcpp/String.h"  // for String
#include <string>

CV icd9AddLeadingZeroes(CV icd9, bool isShort);
CV icd9AddLeadingZeroesShort(CV icd9Short);
Rcpp::String icd9AddLeadingZeroesMajorSingle(Rcpp::String major);
std::string icd9AddLeadingZeroesMajorSingleStd(std::string m);
CV icd9AddLeadingZeroesMajor(CV mjr);

#endif /* MANIP_H_ */
