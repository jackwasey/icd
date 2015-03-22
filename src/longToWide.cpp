// [[Rcpp::interfaces(r, cpp)]]
#include <local.h>
#include <stdio.h>
#ifdef ICD9_VALGRIND
#include <valgrind/callgrind.h>
#endif

using namespace Rcpp;

CharacterVector raggedToWide(const VecVecStr& ragged, int max_per_pt,
		const VecStr &visitIds) {
#ifdef ICD9_DEBUG_TRACE
	Rcpp::Rcout << "visitIds = ";
	printIt(visitIds);
#endif
	VecStr::size_type distinct_visits = ragged.size();
	CharacterVector out(distinct_visits * max_per_pt, NA_STRING); // optionally default empty strings? NA? User can do this for now.
#ifdef ICD9_DEBUG
			if (distinct_visits==0) {
				Rcpp::Rcout << "no visits. returning blank data\n";
				return CharacterVector::create();
			}
			if (distinct_visits != visitIds.size()) {
				Rcpp::Rcout << "visit and ragged sizes differ. visits = " << visitIds.size() << ", ragged size = " << distinct_visits << ": returning blank data\n";
				return CharacterVector::create();
			}
#endif
	for (VecVecStr::size_type row_it = 0; row_it < distinct_visits; ++row_it) {
		const VecStr& this_row = ragged[row_it];
		VecStr::size_type this_row_len = this_row.size();
		for (VecStr::size_type col_it = 0; col_it < this_row_len; ++col_it) {
			VecVecStr::size_type out_idx = row_it + (distinct_visits * col_it); // straight to row major
			// TODO: someday it may be faster to write out column major, then transpose afterwards.
			out[out_idx] = this_row[col_it];
		}
	}
#ifdef ICD9_DEBUG
	Rcpp::Rcout << "writing dimensions\n";
#endif
	out.attr("dim") = Dimension(distinct_visits, max_per_pt); // set dimensions in reverse (row major for parallel step)
#ifdef ICD9_DEBUG
			Rcpp::Rcout << "writing labels\n";
#endif
	CharacterVector nonames;
	rownames(out) = wrap(visitIds);
	return out;
}

int longToRagged(const SEXP& icd9df, VecVecStr& ragged, VecStr& visitIds,
		const std::string visitId, const std::string icd9Field =
				"icd9", bool aggregate = true) {
#ifdef ICD9_VALGRIND
	CALLGRIND_START_INSTRUMENTATION;
#endif
	SEXP icds = PROTECT(getRListOrDfElement(icd9df, icd9Field.c_str()));
	SEXP vsexp = PROTECT(getRListOrDfElement(icd9df, visitId.c_str()));
	const int approx_cmb_per_visit = 15; // just an estimate. Prob best to overestimate.
	int vlen = Rf_length(icds);
	visitIds.reserve(vlen / approx_cmb_per_visit);
	ragged.reserve(vlen / approx_cmb_per_visit);
	int max_per_pt = 1;
	if (TYPEOF(vsexp) != STRSXP)
		Rcpp::stop("need string input to longToRagged\n");
#ifdef ICD9_DEBUG
	Rcpp::Rcout << "longToRagged SEXP is STR\n";
#endif

	const char* lastVisitId = "";
	for (int i = 0; i < vlen; ++i) {
		const char* icd = CHAR(STRING_ELT(icds, i)); // always STRING? may get pure numeric/integer
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
#ifdef ICD9_DEBUG
			if (ragged.size()==0) {
				Rcout << "ragged size is ZERO! aborting\n";
				break;
			}
#endif
			ragged[ragged.size() - 1].push_back(icd); // augment vec for current visit and N/V/E type // EXPENSIVE.
			int len = ragged[ragged.size() - 1].size(); // get new count of cmb for one patient
			if (len > max_per_pt)
				max_per_pt = len;
		}
#ifdef ICD9_DEBUG_TRACE
		Rcout << "ragged size is " << ragged.size() << "\n";
#endif

		lastVisitId = vi;
	} // end loop through all visit-code input data

#ifdef ICD9_VALGRIND
	CALLGRIND_STOP_INSTRUMENTATION;
	//        CALLGRIND_DUMP_STATS;
#endif
	UNPROTECT(2); // do sooner if possible?
	return max_per_pt;
}

// [[Rcpp::export]]
CharacterVector icd9LongToWideCpp(const SEXP& icd9df,
		const std::string visitId, const std::string icd9Field,
		bool aggregate = true) {

	// a few options here. character matrix would make sense, but this can't be a factor. data frame harder to process on C++ side.
	// Matrix easy to convert to data frame in R, if needed. Rows in output don't correspond to input, so no strong reason to preserve factors in any way.
	// therefore, we will only accept strings as input at the C++ level. R wrapper function can pre-convert these.

	VecVecStr ragged; // reserve size later
	int max_per_pt = 0;

	const SEXP vsexp = PROTECT(getRListOrDfElement(icd9df, visitId.c_str()));
	UNPROTECT(1);
	if (TYPEOF(vsexp) != STRSXP)
		Rcpp::stop(
				"visitIds should be pre-converted to str - which is necessary for matrix rowname output anyway");

	VecStr visitIds; // may be vector of integers or strings
#ifdef ICD9_DEBUG
	Rcout << "doing long to ragged\n";
#endif
	max_per_pt = longToRagged(icd9df, ragged, visitIds, visitId, icd9Field,
			aggregate);
#ifdef ICD9_DEBUG_TRACE
	Rcout << "ragged size returned is " << ragged.size() << "\n";
#endif

#ifdef ICD9_DEBUG
	Rcout << "returning ragged to wide\n";
#endif
	return raggedToWide(ragged, max_per_pt, visitIds);
}
