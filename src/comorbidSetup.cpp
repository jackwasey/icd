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
#include "config.h"                            // for valgrind etc
#include "local.h"                            // for VisLk
#include "icd_types.h"                        // for VecInt, VecVecInt, VecV...
#include <Rcpp.h>
#include <algorithm>
#include <map>                                // for _Rb_tree_iterator
#include <string>                             // for string, basic_string
#include <utility>                            // for make_pair, pair
#include <vector>                             // for vector
#include <cstring>
extern "C" {
#include "cutil.h"                            // for getRListOrDfElement
}


// take an R list of factors of ICD codes, and convert to a sorted std vector of
// vector of integers. May not be sorted already because I use factor_nosort.
void buildMap(const Rcpp::List& icd9Mapping, VecVecInt& map) {
#ifdef ICD_DEBUG_SETUP
  Rcpp::Rcout << "creating reference comorbidity mapping STL structure\n";
#endif
  for (Rcpp::List::const_iterator mi = icd9Mapping.begin(); mi != icd9Mapping.end(); ++mi) {
    VecInt vec(Rcpp::as<VecInt>(*mi));
    std::sort(vec.begin(), vec.end());
    map.push_back(vec);
  }
#ifdef ICD_DEBUG_SETUP
  Rcpp::Rcout << "reference comorbidity mapping STL structure created\n";
#endif
}

// icd9 codes always strings, but could be factors. visitId may be factor or
// integer, but ultimately it becomes a string vector (as matrix row names)
void buildVisitCodesVec(const SEXP& icd9df,
                        const std::string& visitId,
                        const std::string& icd9Field,
                        VecVecInt& vcdb,
                        VecStr& visitIds) {
  SEXP icds = PROTECT(getRListOrDfElement(icd9df, icd9Field.c_str()));
  SEXP vsexp = PROTECT(getRListOrDfElement(icd9df, visitId.c_str()));
  const int approx_cmb_per_visit = 15; // just a moderate over-estimate
  int vlen = Rf_length(icds); // or vsexp
  VisLk vis_lookup;

#ifdef ICD_DEBUG_SETUP
  Rcpp::Rcout << "vcdb resized to length vlen = " << vlen << "\n";
#endif
  vcdb.resize(vlen); // over-estimate and allocate all at once (alternative is to reserve)
  VecVecIntSz vcdb_max_idx = -1; // we increment immediately to zero as first index, although this is probably bad form because it is an unsigned value.
  VecVecIntSz vcdb_new_idx;
  // 2094967295 is a random number less than 2^31 (to avoid 32bit R build
  // warning) just to initialize: should always been initialized, though.
  VecVecIntSz vcdb_last_idx = 2094967295;
#ifdef ICD_DEBUG_SETUP
  Rcpp::Rcout << "buildVisitCodes SEXP is STR\n";
  Rcpp::Rcout << "length vcdb = " << vcdb.size() << "\n";
#endif
  visitIds.resize(vlen); // resize and trim at end, as alternative to reserve
  std::string lastVisit = "ac474a402000c1e79c51c3682a952295da0eaa67"; // random long string. std::string probably slightly slower, but safe for arbitrary length visit ids.
  int n;
  for (int i = 0; i != vlen; ++i) {
    const char* vi = CHAR(STRING_ELT(vsexp, i));
    n = INTEGER(icds)[i];
#ifdef ICD_DEBUG_SETUP_TRACE
    Rcpp::Rcout << "building visit: it = " << i << ", id = " << vi << "\n";
#endif
    if (lastVisit != vi) {
      // don't assume new visit id, although this is now likely
      vcdb_new_idx = vcdb_max_idx + 1;
      VisLk::iterator found = vis_lookup.find(vi);
      if (found != vis_lookup.end()) {
        vcdb[found->second].push_back(n); // found row, add a 'column'
#ifdef ICD_DEBUG_SETUP_TRACE
        Rcpp::Rcout << "repeat key " << vi << " found at position " << vcdb_new_idx << "\n";
#endif
        continue;
      } else {
        vis_lookup.insert(
          std::make_pair(vi, vcdb_new_idx)); // new visit, with associated position in vcdb
#ifdef ICD_DEBUG_SETUP_TRACE
        Rcpp::Rcout << "new key " << vi << "\n";
#endif
      }
      // all code paths now add a new row
      vcdb[vcdb_new_idx].reserve(approx_cmb_per_visit);
      vcdb[vcdb_new_idx].push_back(n); // augment vec for current visit
      visitIds[vcdb_new_idx] = vi; // this now seems wasteful having a map AND a vector of these.
      lastVisit = vi;
      vcdb_last_idx = vcdb_new_idx;
      ++vcdb_max_idx;
    } else {
      vcdb[vcdb_last_idx].push_back(n);
#ifdef ICD_DEBUG_SETUP_TRACE
      Rcpp::Rcout << "repeat id found on next row: " << vi << "\n";
#endif
    }
  } // end loop through all visit-code input data
#ifdef ICD_DEBUG_SETUP
  Rcpp::Rcout << "visit map created:" << std::endl;
  for (const VecInt &v : vcdb) {
    printIt(v);
  }
#endif
  UNPROTECT(2);
  vcdb.resize(vcdb_max_idx + 1); // we over-sized (not just over-reserved) so now we trim.
  visitIds.resize(vcdb_max_idx + 1); // we over-sized (not just over-reserved) so now we trim.
}

