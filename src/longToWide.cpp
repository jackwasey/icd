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

#include "icd_types.h"                  // for VecStr, VecVecStr, CV
#include "local.h"
#include <string.h>                     // for strcmp
#include <algorithm>                    // for find, fill
#include <string>                       // for string, operator==
#include <vector>                       // for vector

#ifdef ICD_VALGRIND
#include <valgrind/callgrind.h>
#endif

extern "C" {
#include "cutil.h"
}

CV raggedToWide(const VecVecStr& ragged, int max_per_pt,
                const VecStr &ids) {
#ifdef ICD_DEBUG_TRACE
  Rcpp::Rcout << "ids = ";
  // printIt(ids); // broken, not sure why.
#endif
  VecStr::size_type distinct_visits = ragged.size();
  // optionally default empty strings? NA? User can do this for now.
  CV out(distinct_visits * max_per_pt, NA_STRING);
#ifdef ICD_DEBUG
  assert(distinct_visits > 0)
    assert(distinct_visits == ids.size())
#endif
    for (VecVecStr::size_type row_it = 0; row_it < distinct_visits; ++row_it) {
      const VecStr& this_row = ragged[row_it];
      VecStr::size_type this_row_len = this_row.size();
      for (VecStr::size_type col_it = 0; col_it < this_row_len; ++col_it) {
        // write in row major format, but this means transpose needed later
        VecVecStr::size_type out_idx = row_it + (distinct_visits * col_it);
        out[out_idx] = this_row[col_it];
      } // end inner for
      Rcpp::checkUserInterrupt();
    } // end outer for
    // set dimensions in reverse (row major for parallel step)
    out.attr("dim") = Rcpp::Dimension(distinct_visits, max_per_pt);
  CV nonames;
  rownames(out) = Rcpp::wrap(ids);
  return out;
}

int longToRagged(const SEXP& x, VecVecStr& ragged, VecStr& ids,
                 const std::string id_name, const std::string code_name =
                   "code", bool aggregate = true) {
  SEXP icds = PROTECT(getRListOrDfElement(x, code_name.c_str()));
  SEXP vsexp = PROTECT(getRListOrDfElement(x, id_name.c_str()));
  const int approx_cmb_per_visit = 15; // just an estimate. Prob best to overestimate.
  int vlen = Rf_length(icds);
  ids.reserve(vlen / approx_cmb_per_visit);
  ragged.reserve(vlen / approx_cmb_per_visit);
  int max_per_pt = 1;
  const char* lastid_name = "";
  for (int i = 0; i < vlen; ++i) {
    // always STRING? may get numeric, integer, factor? Can always handle this on R side
    const char* icd = CHAR(STRING_ELT(icds, i));
    const char* vi = CHAR(STRING_ELT(vsexp, i));
    if (strcmp(lastid_name, vi) != 0
          && (!aggregate
                || std::find(ids.rbegin(), ids.rend(), vi)
                == ids.rend())) {
                VecStr vcodes;
      vcodes.reserve(approx_cmb_per_visit); // estimate of number of codes per patient.
      vcodes.push_back(icd); // new vector of ICD codes with this first item
      ragged.push_back(vcodes); // and add that vector to the intermediate structure
      ids.push_back(vi);
    } else {
      assert(ragged.size() != 0);
      ragged[ragged.size() - 1].push_back(icd); // augment vec for current visit and N/V/E type // EXPENSIVE.
      int len = ragged[ragged.size() - 1].size(); // get new count of cmb for one patient
      if (len > max_per_pt)
        max_per_pt = len;
    }
    lastid_name = vi;
    Rcpp::checkUserInterrupt();
  } // end loop through all visit-code input data
  UNPROTECT(2);
  return max_per_pt;
}

// [[Rcpp::export(long_to_wide_cpp)]]
CV longToWideCpp(const SEXP& x, const std::string id_name,
                 const std::string code_name, bool aggregate = true) {
  VecVecStr ragged; // reserve size later
  int max_per_pt = 0;
  const SEXP vsexp = PROTECT(getRListOrDfElement(x, id_name.c_str()));
  if (TYPEOF(vsexp) != STRSXP)
    Rcpp::stop(
      "The contents of the visit ID column should be converted to character.");
  UNPROTECT(1);
  VecStr ids; // may be vector of integers or strings
  max_per_pt = longToRagged(x, ragged, ids, id_name, code_name,
                            aggregate);
  DEBUG_VEC(ragged);
  return raggedToWide(ragged, max_per_pt, ids);
}
