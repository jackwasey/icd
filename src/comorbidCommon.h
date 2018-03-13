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

//#include <vector>                      // for vector, vector<>::const_iterator
#include "icd_types.h"                 // for ComorbidOut, VecVecInt, VecVec...
//#include "local.h"                     // for ICD_OPENMP
//#include "config.h"                     // for valgrind, CXX11 etc

void lookupComorbidByChunkFor(const VecVecInt& vcdb,
                              const VecVecInt& map,
                              const VecVecIntSz chunkSize,
                              const VecVecIntSz ompChunkSize,
                              VecInt& out);
void lookupComorbidByChunkForTaskloop(const VecVecInt& vcdb,
                                      const VecVecInt& map,
                                      VecVecBool& out);
