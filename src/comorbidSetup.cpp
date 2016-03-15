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

// [[Rcpp::interfaces(r, cpp)]]
#include "local.h"
#include <Rcpp.h>
#ifdef ICD9_STD_PARALLEL
#include <parallel/algorithm>
#else
#include <algorithm>
#endif

void buildMap(const Rcpp::List& icd9Mapping, VecVecInt& map) {
  for (Rcpp::List::const_iterator mi = icd9Mapping.begin(); mi != icd9Mapping.end();
  ++mi) {
    VecInt vec(Rcpp::as<VecInt>(*mi));
    std::sort(vec.begin(), vec.end());
    // to force a call to parallel version, not sure whether this forces
    // parallel execution or just starts the heuristics which may invoke
    // parallel execution:
    //__gnu_parallel::sort(vec.begin(), vec.end());
#ifdef ICD9_DEBUG_SETUP_TRACE
    Rcpp::Rcout << "pushing back vec of length: " << vec.size() << "\n";
#endif
    map.push_back(vec);
#ifdef ICD9_DEBUG_SETUP_TRACE
    Rcpp::Rcout << "last vec pushed back has length: "
                << map[map.size() - 1].size() << "\n";
#endif
  }
#ifdef ICD9_DEBUG_SETUP
  Rcpp::Rcout << "reference comorbidity mapping STL structure created\n";
#endif
}

// R CMD INSTALL --no-build-vignettes icd && R -d gdb -e "library(icd);x<-data.frame(visitId=c('a','b'),icd9=c('1','2'));icd9ComorbidAhrq(x)"

// icd9 codes always strings. visitId may be factor or integer, but ultimately it becomes a string vector (as matrix row names)
void buildVisitCodesVec(const SEXP& icd9df,
                        const std::string& visitId,
                        const std::string& icd9Field,
                        VecVecInt& vcdb,
                        VecStr& visitIds,
                        const bool aggregate = true) {
  SEXP icds = PROTECT(getRListOrDfElement(icd9df, icd9Field.c_str()));
  SEXP vsexp = PROTECT(getRListOrDfElement(icd9df, visitId.c_str()));
  const int approx_cmb_per_visit = 15; // just an estimate
  int vlen = Rf_length(icds); // or vsexp

#ifdef ICD9_DEBUG
#ifdef HAVE_CXX11
  Rcpp::Rcout << "unordered_map is available (or at least C++11 is in some form)\n";
#else
  Rcpp::Rcout << "unordered_map is not available\n";
#endif
#endif
  VisLk vis_lookup;

#ifdef ICD9_DEBUG
  Rcpp::Rcout << "vcdb resized to length vlen = " << vlen << "\n";
#endif
  vcdb.resize(vlen); // over-estimate and allocate all at once (alternative is to reserve)
  VecVecIntSz vcdb_max_idx = -1; // we increment immediately to zero as first index
  VecVecIntSz vcdb_new_idx;
  // 2094967295 is a random number less than 2^31 (to avoid 32bit R build
  // warning) just to initialize: should always been initialized, though.
  VecVecIntSz vcdb_last_idx = 2094967295;
#ifdef ICD9_DEBUG_SETUP
  Rcpp::Rcout << "buildVisitCodes SEXP is STR\n";
#endif
  visitIds.resize(vlen); // resize and trim at end, as alternative to reserve
  const char* lastVisitId = "JJ94967295JJ"; // random
  int n;
  for (int i = 0; i != vlen; ++i) {
    const char* vi = CHAR(STRING_ELT(vsexp, i));
    n = INTEGER(icds)[i];
#ifdef ICD9_DEBUG_SETUP_TRACE
    Rcpp::Rcout << "building visit: it = " << i << ", id = " << vi << "\n";
    Rcpp::Rcout << "length vcdb = " << vcdb.size() << "\n";
#endif
    if (lastVisitId != vi) {
      // assume new visitId unless aggregating
      vcdb_new_idx = vcdb_max_idx + 1;
      if (aggregate) { // only use map if aggregating
        VisLk::iterator found = vis_lookup.find(vi); // TODO make const?
        if (found != vis_lookup.end()) {
          vcdb[found->second].push_back(n);
#ifdef ICD9_DEBUG_SETUP_TRACE
          Rcpp::Rcout << "repeat key " << vi << " found at position " << vcdb_new_idx << "\n";
#endif
          continue;
        } else {
          vis_lookup.insert(
            std::make_pair(vi, vcdb_new_idx)); // new visit, with associated position in vcdb
#ifdef ICD9_DEBUG_SETUP_TRACE
          Rcpp::Rcout << "(aggregating) new key " << vi << "\n";
#endif
        }
      } else {
#ifdef ICD9_DEBUG_SETUP_TRACE
        Rcpp::Rcout << "(not aggregating) new key " << vi << "\n";
#endif
      }
      // all code paths now add a new row
      vcdb[vcdb_new_idx].reserve(approx_cmb_per_visit);
      vcdb[vcdb_new_idx].push_back(n); // augment vec for current visit
      visitIds[vcdb_new_idx] = vi; // this now seems wasteful having a map AND a vector of these.
      lastVisitId = vi;
      vcdb_last_idx = vcdb_new_idx;
      ++vcdb_max_idx;
    } else {
      vcdb[vcdb_last_idx].push_back(n);
#ifdef ICD9_DEBUG_SETUP_TRACE
      Rcpp::Rcout << "repeat id found on next row: " << vi << "\n";
#endif
    }
  } // end loop through all visit-code input data
#ifdef ICD9_DEBUG_SETUP
  Rcpp::Rcout << "visit map created\n";
#endif
  UNPROTECT(2);
  vcdb.resize(vcdb_max_idx + 1); // we over-sized (not just over-reserved) so now we trim.
  visitIds.resize(vcdb_max_idx + 1); // we over-sized (not just over-reserved) so now we trim.
}

