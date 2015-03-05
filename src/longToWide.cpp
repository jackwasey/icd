// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include <R.h>
#include <Rinternals.h>
#include <icd9.h>
#include <local.h>
#include <stdio.h>
#ifdef ICD9_VALGRIND
#include <valgrind/callgrind.h>
#endif

extern "C" {
#include "local_c.h"
#include <cstdlib>
}
using namespace Rcpp;

CharacterVector raggedWideMultimapToMatrix(const MMVisitCodes &mm,
		const int max_per_pt) {
#ifdef ICD9_DEBUG_SETUP
	//std::cout << "visitIds = ";
	//printIt(visitIds);
#endif
	MMVisitCodes::size_type distinct_visits = mm.size();
	CharacterVector out(distinct_visits * max_per_pt); // default empty strings? NA? //TODO
	for (MMVisitCodes::const_iterator it = mm.begin(); it != mm.end(); ++it) {
		unsigned int this_row_len = (it->second).size();
		const unsigned int row_idx = std::distance(mm.begin(), it);
		for (unsigned int col_idx = 0; col_idx < this_row_len; ++col_idx) {
			unsigned int out_idx = row_idx + (distinct_visits * col_idx); // straight to row major //TODO benchmark alternative with transposition
			out[out_idx] = (it->second)[col_idx];
		}
	}
#ifdef ICD9_DEBUG_SETUP
	std::cout << "writing dimensions\n";
#endif
	out.attr("dim") = Dimension(distinct_visits, max_per_pt); // set dimensions in reverse (row major for parallel step)
#ifdef ICD9_DEBUG_SETUP
			std::cout << "writing labels\n";
#endif
	CharacterVector nonames;
	//rownames(out) = wrap(visitIds);
	return out;
}

// THIS IS SLOWER THAN THE non-map version, when the patients are ordered or nearly ordered.
// [[Rcpp::export]]
CharacterVector icd9LongToWideMatrixByMap(const SEXP& icd9df,
		const std::string visitId = "visitId", const std::string icd9Field =
				"icd9") {
#ifdef ICD9_VALGRIND
	CALLGRIND_START_INSTRUMENTATION;
#endif

#ifdef ICD9_DEBUG_SETUP
	std::cout << "calling C to get icd codes\n";
#endif
	SEXP icds = PROTECT(getRListOrDfElement(icd9df, icd9Field.c_str())); // very fast
#ifdef ICD9_DEBUG_SETUP
			std::cout << "back from C\n";
#endif
	// very slow, but probably necessary, because we are going to be manipulating them more. Could we go straight from SEXP to VecStr?
	VecStr vs = as<VecStr>(
			as<CharacterVector>(getRListOrDfElement(icd9df, visitId.c_str()))); // TODO can we do this in one step without Rcpp copying?
	const unsigned int approx_cmb_per_visit = 15; // just an estimate. probably better to overestimate significantly.
#ifdef ICD9_DEBUG_SETUP
	std::cout << "getting length of icd codes\n";
#endif
	unsigned int vlen = Rf_length(icds);
	MMVisitCodes visitCodes;
	unsigned int max_per_pt = 1;
	for (unsigned int i = 0; i < vlen; ++i) {
#ifdef ICD9_DEBUG_SETUP_TRACE
		std::cout << "calling R C function to get current ICD...";
#endif
		const char* s = CHAR(STRING_ELT(icds, i));
#ifdef ICD9_DEBUG_SETUP_TRACE
		std::cout << " and got value: " << s << "\n";
		std::cout << "visitIds = ";
		//printIt(visitIds);
		std::cout << "Current visitId: " << vs[i] << "\n";
#endif

		MMVisitCodes::iterator found_it = visitCodes.find(vs[i]);
		if (found_it != visitCodes.end()) {
#ifdef ICD9_DEBUG_SETUP_TRACE
			std::cout << "out-of-sequence repeat id found: " << vs[i] << "\n";
			std::cout << "map size: " << visitCodes.size() << "\n";
#endif
			(found_it->second).push_back(s); // augment vec for current visit and N/V/E type
			unsigned int len = (found_it->second).size(); // get new count of cmb for one patient
			if (len > max_per_pt)
				max_per_pt = len;
		} else {
#ifdef ICD9_DEBUG_SETUP_TRACE
			std::cout << "new key " << vs[i] << "\n";
#endif
			VecStr vcodes;
			vcodes.reserve(approx_cmb_per_visit); // estimate of number of codes per patient. // this is a little expensive
			vcodes.push_back(s); // new vector of ICD codes with this first item
			visitCodes.insert(std::make_pair(vs[i], vcodes));
		} // end find
	} // end loop through all visit-code input data
#ifdef ICD9_DEBUG_SETUP
	std::cout << "intermediate ragged-right map created\n";
#endif
	CharacterVector cv = raggedWideMultimapToMatrix(visitCodes, max_per_pt);
#ifdef ICD9_VALGRIND
	CALLGRIND_STOP_INSTRUMENTATION;
	CALLGRIND_DUMP_STATS;
#endif
	UNPROTECT(1);
	return cv;
}

CharacterVector raggedToWide(const VecVecStr& ragged, int max_per_pt,
		const VecStr &visitIds) {
#ifdef ICD9_DEBUG_TRACE
	std::cout << "visitIds = ";
	printIt(visitIds);
#endif
	VecStr::size_type distinct_visits = ragged.size();
	CharacterVector out(distinct_visits * max_per_pt, NA_STRING); // default empty strings? NA? //TODO
#ifdef ICD9_DEBUG
			if (distinct_visits==0) {
				std::cout << "no visits. returning blank data\n";
				return CharacterVector::create();
			}
			if (distinct_visits != visitIds.size()) {
				std::cout << "visit and ragged sizes differ. visits = " << visitIds.size() << ", ragged size = " << distinct_visits << ": returning blank data\n";
				return CharacterVector::create();
			}
#endif
	for (VecVecStr::size_type row_it = 0; row_it < distinct_visits; ++row_it) {
		const VecStr& this_row = ragged[row_it];
		VecStr::size_type this_row_len = this_row.size();
		for (VecStr::size_type col_it = 0; col_it < this_row_len; ++col_it) {
			unsigned int out_idx = row_it + (distinct_visits * col_it); // straight to row major //TODO benchmark alternative with transposition
			out[out_idx] = this_row[col_it];
		}
	}
#ifdef ICD9_DEBUG
	std::cout << "writing dimensions\n";
#endif
	out.attr("dim") = Dimension(distinct_visits, max_per_pt); // set dimensions in reverse (row major for parallel step)
#ifdef ICD9_DEBUG
			std::cout << "writing labels\n";
#endif
	CharacterVector nonames;
	rownames(out) = wrap(visitIds);
	return out;
}

int longToRagged(const SEXP& icd9df, VecVecStr& ragged, VecStr& visitIds,
		const std::string visitId = "visitId", const std::string icd9Field =
				"icd9", bool aggregate = true) {
#ifdef ICD9_VALGRIND
	CALLGRIND_START_INSTRUMENTATION;
#endif
	SEXP icds = PROTECT(getRListOrDfElement(icd9df, icd9Field.c_str()));
	SEXP vsexp = PROTECT(getRListOrDfElement(icd9df, visitId.c_str()));
	const int approx_cmb_per_visit = 15; // just an estimate. Prob best to overestimate.
	int vlen = Rf_length(icds);
	visitIds.reserve(vlen / approx_cmb_per_visit);
	int max_per_pt = 1;
	if (TYPEOF(vsexp) != STRSXP)
		stop("need string input to longToRagged\n");
#ifdef ICD9_DEBUG
	std::cout << "longToRagged SEXP is STR\n";
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
			ragged[ragged.size() - 1].push_back(icd); // augment vec for current visit and N/V/E type // EXPENSIVE. TODO try over-reserving more.
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

//' @title Convert long to wide form as matrix
//' @description Take a data frame with visits and ICD codes in two columns, and convert to a matrix with one row per visit.
//' Since multiple rows are combined when visits are out of sequence, no guarantee is made about the returned order. We sort implicitly.
//' For guaranteed order, we can't de-duplicate disordered visitIds, just aggregate contiguous blocks: icd9LongOrderedToWide does this quickly.
//' @export
// [[Rcpp::export]]
CharacterVector icd9LongToWideCpp(const SEXP& icd9df,
		const std::string visitId = "visitId", const std::string icd9Field =
				"icd9", bool aggregate = true) {

	// a few options here. character matrix would make sense, but this can't be a factor. data frame harder to process on C++ side.
	// Matrix easy to convert to data frame in R, if needed. Rows in output don't correspond to input, so no strong reason to preserve factors in any way.
	// therefore, we will only accept strings as input at the C++ level. R wrapper function can pre-convert these.

	VecVecStr ragged; // size reserved later.
	int max_per_pt = 0;

	const SEXP vsexp = PROTECT(getRListOrDfElement(icd9df, visitId.c_str()));
	UNPROTECT(1);
	if (TYPEOF(vsexp) != STRSXP)
		stop(
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
