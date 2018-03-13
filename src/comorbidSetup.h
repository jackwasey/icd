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

// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include <Rcpp/r/headers.h>                   // for INTEGER, Rf_length, SEXP
#ifdef ICD_STD_PARALLEL
#include <parallel/algorithm>
#else
#include <algorithm>
#endif
#include <map>                                // for _Rb_tree_iterator
#include <string>                             // for string, basic_string
#include <utility>                            // for make_pair, pair
#include <vector>                             // for vector
#include "Rcpp/as.h"                          // for as
#include "Rcpp/vector/Vector.h"               // for Vector<>::const_iterator
#include "Rcpp/vector/const_generic_proxy.h"  // for const_generic_proxy
#include "Rcpp/vector/instantiation.h"        // for List
#include "RcppCommon.h"                       // for Proxy_Iterator
#include "icd_types.h"                        // for VecInt, VecVecInt, VecV...
#include "local.h"                            // for VisLk
#include "config.h"                            // for valgrind etc
extern "C" {
#include "cutil.h"                            // for getRListOrDfElement
}

void buildMap(const Rcpp::List& icd9Mapping, VecVecInt& map);
void buildVisitCodesVec(const SEXP& icd9df,
                        const std::string& visitId,
                        const std::string& icd9Field,
                        VecVecInt& vcdb,
                        VecStr& visitIds,
                        const bool aggregate = true);
