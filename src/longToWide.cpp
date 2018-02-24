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
#include <Rcpp/r/headers.h>             // for STRING_ELT, SEXP, PROTECT
#include <string.h>                     // for strcmp
#include <algorithm>                    // for find, fill
#include <string>                       // for string, operator==
#include <vector>                       // for vector
#include "Rcpp.h"                       // for wrap
#include "Rcpp/Dimension.h"             // for Dimension
#include "Rcpp/api/meat/Dimension.h"    // for Dimension::operator SEXPREC *
#include "Rcpp/api/meat/proxy.h"        // for AttributeProxyPolicy::Attribu...
#include "Rcpp/exceptions.h"            // for stop
#include "Rcpp/proxy/AttributeProxy.h"  // for AttributeProxyPolicy<>::Attri...
#include "Rcpp/vector/DimNameProxy.h"   // for DimNameProxy
#include "Rcpp/vector/Matrix.h"         // for rownames
#include "Rcpp/vector/Vector.h"         // for Vector<>::Proxy
#include "icd_types.h"                  // for VecStr, VecVecStr, CV

#ifdef ICD_VALGRIND
#include <valgrind/callgrind.h>
#endif

extern "C" {
  #include "cutil.h"                            // for getRListOrDfElement
}

CV raggedToWide(const VecVecStr& ragged, int max_per_pt,
		const VecStr &visitIds) {
#ifdef ICD_DEBUG_TRACE
	Rcpp::Rcout << "visitIds = ";
	// printIt(visitIds); // broken, not sure why.
#endif
	VecStr::size_type distinct_visits = ragged.size();
	CV out(distinct_visits * max_per_pt, NA_STRING); // optionally default empty strings? NA? User can do this for now.
#ifdef ICD_DEBUG
			if (distinct_visits == 0) {
				Rcpp::Rcout << "no visits. returning blank data\n";
				return CV::create();
			}
			if (distinct_visits != visitIds.size()) {
				Rcpp::Rcout << "visit and ragged sizes differ. visits = " << visitIds.size() <<
				  ", ragged size = " << distinct_visits << ": returning blank data\n";
				return CV::create();
			}
#endif
	for (VecVecStr::size_type row_it = 0; row_it < distinct_visits; ++row_it) {
		const VecStr& this_row = ragged[row_it];
		VecStr::size_type this_row_len = this_row.size();
		for (VecStr::size_type col_it = 0; col_it < this_row_len; ++col_it) {
		  // write in row major format, but this means transpose needed later
			VecVecStr::size_type out_idx = row_it + (distinct_visits * col_it);
			out[out_idx] = this_row[col_it];
		}
	}
#ifdef ICD_DEBUG
	Rcpp::Rcout << "writing dimensions\n";
#endif
			// set dimensions in reverse (row major for parallel step)
	out.attr("dim") = Rcpp::Dimension(distinct_visits, max_per_pt);
#ifdef ICD_DEBUG
			Rcpp::Rcout << "writing labels\n";
#endif
			CV nonames;
	rownames(out) = Rcpp::wrap(visitIds);
	return out;
}

int longToRagged(const SEXP& icd9df, VecVecStr& ragged, VecStr& visitIds,
		const std::string visitId, const std::string icd9Field =
				"code", bool aggregate = true) {
#ifdef ICD_VALGRIND
	CALLGRIND_START_INSTRUMENTATION;
#endif
	SEXP icds = PROTECT(getRListOrDfElement(icd9df, icd9Field.c_str()));
	SEXP vsexp = PROTECT(getRListOrDfElement(icd9df, visitId.c_str()));
	const int approx_cmb_per_visit = 15; // just an estimate. Prob best to overestimate.
	int vlen = Rf_length(icds);
	visitIds.reserve(vlen / approx_cmb_per_visit);
	ragged.reserve(vlen / approx_cmb_per_visit);
	int max_per_pt = 1;

	const char* lastVisitId = "";
	for (int i = 0; i < vlen; ++i) {
	  // always STRING? may get numeric, integer, factor? Can always handle this on R side
		const char* icd = CHAR(STRING_ELT(icds, i));
		const char* vi = CHAR(STRING_ELT(vsexp, i));

		if (strcmp(lastVisitId, vi) != 0
				&& (!aggregate
						|| std::find(visitIds.rbegin(), visitIds.rend(), vi)
								== visitIds.rend())) {
			VecStr vcodes;
			vcodes.reserve(approx_cmb_per_visit); // estimate of number of codes per patient.
			vcodes.push_back(icd); // new vector of ICD codes with this first item
			ragged.push_back(vcodes); // and add that vector to the intermediate structure
			visitIds.push_back(vi);
		} else {
#ifdef ICD_DEBUG
			if (ragged.size() == 0) {
			  Rcpp::Rcout << "ragged size is ZERO! bailing ou!\n";
				break;
			}
#endif
			ragged[ragged.size() - 1].push_back(icd); // augment vec for current visit and N/V/E type // EXPENSIVE.
			int len = ragged[ragged.size() - 1].size(); // get new count of cmb for one patient
			if (len > max_per_pt)
				max_per_pt = len;
		}
#ifdef ICD_DEBUG_TRACE
		Rcpp::Rcout << "ragged size is " << ragged.size() << "\n";
#endif

		lastVisitId = vi;
	} // end loop through all visit-code input data

#ifdef ICD_VALGRIND
	CALLGRIND_STOP_INSTRUMENTATION;
	//        CALLGRIND_DUMP_STATS;
#endif
	UNPROTECT(2); // do sooner if possible?
	return max_per_pt;
}

// [[Rcpp::export(icd_long_to_wide_cpp)]]
CV icd9LongToWideCpp(const SEXP& icd9df,
		const std::string visitId, const std::string icd9Field,
		bool aggregate = true) {

	// a few options here. character matrix would make sense, but this can't be a factor. data frame harder to process on C++ side.
	// Matrix easy to convert to data frame in R, if needed. Rows in output don't correspond to input, so no strong reason to preserve factors in any way.
	// therefore, we will only accept strings as input at the C++ level. R wrapper function can pre-convert these.

	VecVecStr ragged; // reserve size later
	int max_per_pt = 0;

	const SEXP vsexp = PROTECT(getRListOrDfElement(icd9df, visitId.c_str()));
	if (TYPEOF(vsexp) != STRSXP)
	  Rcpp::stop(
					"The contents of the visit ID column should be converted to character.");
	UNPROTECT(1);

	VecStr visitIds; // may be vector of integers or strings
#ifdef ICD_DEBUG
	Rcpp::Rcout << "doing long to ragged\n";
#endif
	max_per_pt = longToRagged(icd9df, ragged, visitIds, visitId, icd9Field,
			aggregate);
#ifdef ICD_DEBUG_TRACE
	Rcpp::Rcout << "ragged size returned is " << ragged.size() << "\n";
#endif

#ifdef ICD_DEBUG
	Rcpp::Rcout << "returning ragged to wide\n";
#endif
	return raggedToWide(ragged, max_per_pt, visitIds);
}
