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

#ifndef CONVERT_H_
#define CONVERT_H_

#include "icd_types.h"
using namespace Rcpp;

// need default argument here for other functions to exploit,
// but this is then not exported by Rcpp (which works on the function body).
CV icd9PartsToShort(const List& parts);
CV icd9PartsToDecimal(const List& parts);
List majMinToParts(const CV& mjr, const CV& mnr);
List icd9ShortToParts(const CV& icd9Short, const String mnr_empty = "");
List icd9DecimalToParts(const CV& icd9Decimal, const String mnr_empty = "");
CV icd9DecimalToShort(const CV& icd9Decimal);
CV icd9ShortToDecimal(const CV& icd9Short);
CV icd9GetMajor(const CV& icd9, const bool isShort);

#endif /* CONVERT_H_ */
