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

#ifndef VALGRIND_ICD_H_
#define VALGRIND_ICD_H_

#include "icd_types.h"
#include "local.h"
#include <string>                       // for string
#include <utility>                      // for pair
#include <vector>                       // for vector

int valgrindCallgrindStart(bool zerostats);
int valgrindCallgrindStop();

#endif /* ICD_VALGRIND_ICD_H_ */
