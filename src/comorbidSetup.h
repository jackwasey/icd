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

#ifndef COMORBIDSETUP_H_
#define COMORBIDSETUP_H_

// n.b. Rcpp ignores the ifdef stuff
// [[Rcpp::interfaces(r, cpp)]]
#include "icd_types.h"                        // for VecInt, VecVecInt, VecV...
#include "local.h"                            // for VisLk
#include "config.h"                            // for valgrind etc
#include <map>                                // for _Rb_tree_iterator
#include <string>                             // for string, basic_string
#include <utility>                            // for make_pair, pair
#include <vector>                             // for vector
extern "C" {
#include "cutil.h"                            // for getRListOrDfElement
}

void buildMap(const Rcpp::List& icd9Mapping, VecVecInt& map);
void buildVisitCodesVec(const SEXP& icd9df,
                        const std::string& visitId,
                        const std::string& icd9Field,
                        VecVecInt& vcdb,
                        VecStr& visitIds);

#endif /* COMORBIDSETUP_H_ */
