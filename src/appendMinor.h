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

#ifndef APPENDMINOR_H_
#define APPENDMINOR_H_

#include "icd_types.h"
#include <Rcpp.h>

CV icd9MajMinToCode(const CV mjr, const CV mnr, bool isShort);
CV icd9MajMinToShort(const CV mjr, const CV mnr);
VecStr icd9MajMinToShortStd(const VecStr mjr, const VecStr mnr);
CV icd9MajMinToDecimal(const CV mjr, const CV mnr);

#endif /* APPENDMINOR_H_ */
