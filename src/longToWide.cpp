// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include <R.h>
#include <Rinternals.h>
#include <icd9.h>
#include <local.h>
#ifdef ICD9_VALGRIND
#include <valgrind/callgrind.h>
#endif

extern "C" {
#include "local_c.h"
}
using namespace Rcpp;

template<typename T>
SEXP raggedWideVecVecToMatrix(const std::vector<VecStr>& ragged,
		unsigned int max_per_pt, const T &visitIds) {
#ifdef ICD9_DEBUG_SETUP
	std::cout << "visitIds = ";
	printIt(visitIds);
#endif
	unsigned int distinct_visits = ragged.size();
	CharacterVector out(distinct_visits * max_per_pt); // default empty strings? NA? //TODO
	for (unsigned int row_it = 0; row_it != distinct_visits; ++row_it) {
		const VecStr& this_row = ragged[row_it];
		unsigned int this_row_len = this_row.size();
		for (unsigned int col_it = 0; col_it < this_row_len; ++col_it) {
			unsigned int out_idx = row_it + (distinct_visits * col_it); // straight to row major //TODO benchmark alternative with transposition
			out[out_idx] = this_row[col_it];
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
	rownames(out) = wrap(visitIds);
	return out;
}

SEXP raggedWideMultimapToMatrix(const MMVisitCodes &mm, const unsigned int max_per_pt) {
#ifdef ICD9_DEBUG_SETUP
	//std::cout << "visitIds = ";
	//printIt(visitIds);
#endif
	unsigned int distinct_visits = mm.size();
	CharacterVector out(distinct_visits * max_per_pt); // default empty strings? NA? //TODO
	for (MMVisitCodes::const_iterator it=mm.begin(); it!=mm.end(); ++it) {
		unsigned int this_row_len = (it->second).size();
		const unsigned int row_idx=std::distance(mm.begin(), it);
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
SEXP icd9LongToWideMatrixByMap(const SEXP& icd9df, const std::string visitId =
		"visitId", const std::string icd9Field = "icd9") {
#ifdef ICD9_VALGRIND
	CALLGRIND_START_INSTRUMENTATION;
#endif

#ifdef ICD9_DEBUG_SETUP
	std::cout << "calling C to get icd codes\n";
#endif
	SEXP icds = getListElement(icd9df, icd9Field.c_str()); // very fast
#ifdef ICD9_DEBUG_SETUP
	std::cout << "back from C\n";
#endif
	// very slow, but probably necessary, because we are going to be manipulating them more. Could we go straight from SEXP to VecStr?
	VecStr vs = as<VecStr>(
			as<CharacterVector>(getListElement(icd9df, visitId.c_str()))); // TODO can we do this in one step without Rcpp copying?
	const unsigned int approx_cmb_per_visit = 5; // just an estimate
#ifdef ICD9_DEBUG_SETUP
	std::cout << "getting length of icd codes\n";
#endif
	unsigned int vlen = Rf_length(icds);
	MMVisitCodes visitCodes;
	unsigned int max_per_pt = 1;
	for (unsigned int i=0; i<vlen; ++i) {
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
			vcodes.reserve(approx_cmb_per_visit); // estimate of number of codes per patient.
			vcodes.push_back(s); // new vector of ICD codes with this first item
			visitCodes.insert(std::make_pair(vs[i],vcodes));
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
	return cv;
}

unsigned int longToWideAggregateCore(const char* lastVisitId, const char* icd,
		const char* vi, const unsigned int approx_cmb_per_visit,
		unsigned int max_per_pt, VecStr& visitIds,
		std::vector<VecStr>& ragged) {
	// reverse find might be quicker
	if (lastVisitId != vi
			&& std::find(visitIds.rbegin(), visitIds.rend(), vi)
	== visitIds.rend()) {
		//if (std::find(visitIds.rbegin(), visitIds.rend(), vs[i]) == visitIds.rend()) {
		VecStr vcodes;
		vcodes.reserve(approx_cmb_per_visit); // estimate of number of codes per patient.
		vcodes.push_back(icd); // new vector of ICD codes with this first item
		ragged.push_back(vcodes); // and add that vector to the intermediate structure
		visitIds.push_back(vi);
	} else {
		ragged[ragged.size() - 1].push_back(icd); // augment vec for current visit and N/V/E type
		unsigned int len = ragged[ragged.size() - 1].size(); // get new count of cmb for one patient
		if (len > max_per_pt)
			max_per_pt = len;
	}
	return max_per_pt;
}

// [[Rcpp::export]]
SEXP icd9LongToWideMatrixAggregate(const SEXP& icd9df, const std::string visitId="visitId", const std::string icd9Field="icd9") {
	SEXP icds = getListElement(icd9df, icd9Field.c_str());
	//VecStr vs = as<VecStr>(as<CharacterVector>(getListElement(icd9df, visitId.c_str()))); // TODO do this in one step without Rcpp copying?
	SEXP vsexp = getListElement(icd9df, visitId.c_str());
	const unsigned int approx_cmb_per_visit = 7; // just an estimate
	unsigned int vlen = Rf_length(icds);
	VecStr visitIds;
	visitIds.reserve(vlen / approx_cmb_per_visit);
	std::vector<VecStr> ragged; // intermediate structure
	unsigned int max_per_pt = 1;
	switch(TYPEOF(vsexp)) {
	case INTSXP:
#ifdef ICD9_DEBUG_SETUP
		std::cout << "SEXP is INT\n";
#endif
		{
			int lastVisitId = -999999999;
			for (unsigned int i = 0; i < vlen; ++i) {
				const char* icd = CHAR(STRING_ELT(icds, i)); // always STRING? may get pure numeric/integer
				const int vi = INTEGER(VECTOR_ELT(vsexp, i));
				max_per_pt = longToWideAggregateCore(lastVisitId, icd, vi,
						approx_cmb_per_visit, max_per_pt, visitIds, ragged);

				lastVisitId = vi;
			} // end loop through all visit-code input data
		} // end block
		break;
	case REALSXP:
#ifdef ICD9_DEBUG_SETUP
		std::cout << "SEXP is REAL\n";
#endif
		break;
	case STRSXP:
	{
		const char* lastVisitId = "";
		for (unsigned int i = 0; i < vlen; ++i) {
			const char* icd = CHAR(STRING_ELT(icds, i)); // always STRING? may get pure numeric/integer
			const char* vi = CHAR(STRING_ELT(vsexp, i));
			max_per_pt = longToWideAggregateCore(lastVisitId, icd, vi,
					approx_cmb_per_visit, max_per_pt, visitIds, ragged);

			lastVisitId = vi;
		} // end loop through all visit-code input data
	} // end block
	break;
	default:
		std::cout << "SEXP is other...\n";
		// shouldn't be here...
		break;
	}

	return raggedWideVecVecToMatrix(ragged, max_per_pt, visitIds);
}


//' @title Convert ordered long to wide from as matrix
//' @description This runs only slightly more quickly than the aggregating version when patients are ordered or nearly ordered.
//' @keywords internal
// [[Rcpp::export]]
SEXP icd9LongToWideMatrixNoAggregate(const SEXP& icd9df, const std::string visitId =
		"visitId", const std::string icd9Field = "icd9") {

	SEXP icds = getListElement(icd9df, icd9Field.c_str());
	VecStr vs = as<VecStr>(
			as<CharacterVector>(getListElement(icd9df, visitId.c_str()))); // TODO can we do this in one step without Rcpp copying?
	const unsigned int approx_cmb_per_visit = 7; // just an estimate
	unsigned int vlen = Rf_length(icds);
	VecStr visitIds;
	Str lastVisit = "";
	visitIds.reserve(vlen / approx_cmb_per_visit);
	std::vector<VecStr> ragged; // intermediate structure
	unsigned int max_per_pt = 1;
	Str last_visit;
	for (unsigned int i = 0; i < vlen; ++i) {
		const char* s = CHAR(STRING_ELT(icds, i));
		if (vs[i] != lastVisit) {
			VecStr vcodes;
			vcodes.reserve(approx_cmb_per_visit); // estimate of number of codes per patient.
			vcodes.push_back(s); // new vector of ICD codes with this first item
			ragged.push_back(vcodes); // and add that vector to the intermediate structure
			visitIds.push_back(vs[i]);
			lastVisit = vs[i];
		} else {
			int ragged_end = ragged.size()-1;
			ragged[ragged_end].push_back(s); // augment vec for current visit and N/V/E type
			unsigned int len = ragged[ragged_end].size(); // get new count of cmb for one patient
			if (len > max_per_pt)
				max_per_pt = len;
		}
	} // end loop through all visit-code input data
	return raggedWideVecVecToMatrix(ragged, max_per_pt, visitIds);
}

//' @title Convert long to wide from as matrix
//' @description Take a data frame with visits and ICD codes in two columns, and convert to a matrix with one row per visit.
//' Since multiple rows are combined when visits are out of sequence, no guarantee is made about the returned order. We sort implicitly.
//' For guaranteed order, we can't de-duplicate disordered visitIds, just aggregate contiguous blocks: icd9LongOrderedToWide does this quickly.
//' @export
// [[Rcpp::export]]
SEXP icd9LongToWide(const SEXP& icd9df,
		const std::string visitId="visitId",
		const std::string icd9Field="icd9",
		bool aggregate = true) {
	if (aggregate) return icd9LongToWideMatrixAggregate(icd9df, visitId, icd9Field);
	return icd9LongToWideMatrixNoAggregate(icd9df, visitId, icd9Field);
}
