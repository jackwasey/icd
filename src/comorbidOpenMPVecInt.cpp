// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::plugins(openmp)]]

// R CMD INSTALL --no-docs icd9 && R -e "library(icd9); icd9:::runOpenMPVecInt();"
#ifdef _OPENMP // not available on clang
#include <omp.h>
#endif
#include <Rcpp.h>
#include <R.h>
#include <Rinternals.h>
#include <local.h>
#include <string>
#include <algorithm>
// enable linux performance counting
#ifdef ICD9_VALGRIND
#include <valgrind/callgrind.h>
#endif
extern "C" {
#include "local_c.h"
#include <cstdlib>
}
using namespace Rcpp;

//' @rdname icd9Comorbid
//' @description RcppParallel approach with openmp and vector of integer strategy
//' @param aggregate single logical value, if /code{TRUE}, then take (possible much) more time to aggregate out-of-sequence visit IDs in the icd9df data.frame. If this is \code{FALSE}, then each contiguous group of visit IDs will result in a row of comorbidities in the output data. If you know your visitIds are possible disordered, then use \code{TRUE}.
//' @export
// [[Rcpp::export]]
SEXP icd9ComorbidShortCpp(const SEXP& icd9df, const List& icd9Mapping, const std::string visitId="visitId",
		const std::string icd9Field="icd9", const int threads=8, const int chunkSize=256, const int ompChunkSize=1, bool aggregate=true) {
#ifdef ICD9_VALGRIND
	CALLGRIND_START_INSTRUMENTATION;
#endif
#if (defined ICD9_DEBUG_SETUP || defined ICD9_SETUP)
	std::cout << "icd9ComorbidShortOpenMPVecInt\n";
	std::cout << "chunk size = " << chunkSize << "\n";
#endif

#ifdef ICD9_DEBUG_PARALLEL
	std::cout << "checking _OPENMP... ";
#ifdef _OPENMP
	std::cout << "_OPENMP is defined.\n";
#else
	std::cout << "_OPENMP is not defined.\n";
#endif
#endif

#ifdef _OPENMP
	if (threads > 0)
		omp_set_num_threads(threads);
#ifdef ICD9_DEBUG_PARALLEL
	std::cout << "Max Number of available threads=" << omp_get_max_threads() << "\n";
#endif
#endif

	VecStr out_row_names; // TODO: Reserve size
#ifdef ICD9_DEBUG_SETUP
	std::cout << "building visit:codes structure\n";
#endif

	//const CodesVecSubtype allCodes = buildVisitCodesVecFromMap(icd9df, visitId, icd9Field, _visitIds);
	VecVecInt vcdb; //TODO: reserve sizes

//	if (Rf_isFactor(vsexp)) {
//		#ifdef ICD9_DEBUG_SETUP
//		std::cout << "converting integer factor ids back to names for matrix row names\n";
//#endif
//		SEXP levels = Rf_getAttrib(vsexp, Rf_install("levels"));
//		for (size_t vis_it = 0; vis_it != visitIds.size(); vis_it++) {
//#ifdef ICD9_DEBUG_SETUP_TRACE
//			std::cout << "levels length = " << Rf_length(levels) << "\n";
//		std::cout << "vis_it = " << vis_it << " and visit# = " << visitIds[vis_it] << "\n"; // " and facidx = " << INTEGER(vsexp)[vis_it] << "\n";
//#endif
//			out_row_names.push_back(CHAR(STRING_ELT(levels, visitIds[vis_it]-1)));
//		}
//}

	//TODO: make sure we pre-convert factors and ints to char (or do it here). They will end up being chars anyway as row names.
	// TODO: do I need to allocate memory when I do this?
	SEXP vsexp = PROTECT(getRListOrDfElement(icd9df, visitId.c_str()));
#ifdef ICD9_DEBUG_SETUP
	std::cout << "type of vsexp = " << TYPEOF(vsexp) << "\n";
#endif
	switch(TYPEOF(vsexp)) {
	case INTSXP:
	{
#ifdef ICD9_DEBUG_SETUP
		std::cout << "icd9ComorbidShortMatrix INTSXP\n";
		if (Rf_isFactor(vsexp)) std::cout << "and is a factor\n";
#endif
		VecStr visitIds;
		buildVisitCodesVec(icd9df, visitId, icd9Field, vcdb, visitIds, aggregate);

			//TODO out_row_names = myvecitos(visitIds);

		break;
	}
	case STRSXP:
	{
#ifdef ICD9_DEBUG_SETUP
		std::cout << "icd9ComorbidShortMatrix STRSXP\n";
#endif
		buildVisitCodesVec(icd9df, visitId, icd9Field, vcdb, out_row_names, aggregate);
		break;
	}
	default:
		Rcout << "shouldn't be here in comorbid. TYPEOF is " << TYPEOF(vsexp) << "\n";
	}

#ifdef ICD9_DEBUG_SETUP
	std::cout << "building icd9Mapping\n";
#endif
	// TODO: doing this as a factor would avoid splitting up into three different vectors
	VecVecInt map;
	buildMap(icd9Mapping, map);

#ifdef ICD9_DEBUG_SETUP
	std::cout << "first cmb has len: " << map_n[0].size() << "\n";
#endif

	const VecVecInt::size_type num_comorbid = map.size(); // should be same for V and E
	//const CodesVecSubtype::size_type num_visits = visitIds.size();
	const CodesVecSubtype::size_type num_visits = vcdb.size();

#ifdef ICD9_DEBUG_SETUP
	std::cout << num_visits << " visits\n";
	std::cout << num_comorbid << " is num_comorbid\n";
#endif

	//const Out out = lookupComorbidByRowFor(allCodes, map, chunkSize);
	//const Out out = lookupComorbidByChunkWhile(allCodes, map, chunkSize);
	const Out out = lookupComorbidByChunkFor(vcdb, map, chunkSize, ompChunkSize);

#ifdef ICD9_DEBUG
	std::cout << "out length is " << out.size() << "\n";
	int outsum = std::accumulate(out.begin(), out.end(), 0);
	std::cout << "out sum is " << outsum << "\n";
	std::cout << "Ready to convert to R Matrix\n";
#endif
#ifdef ICD9_TRACE
	std::cout << "out is: ";
	printIt(out);
	std::cout << "printed\n";
#endif
	//IntegerVector mat_out = wrap(out); // matrix is just a vector with dimensions (and col major...) // please don't copy data!
	LogicalVector mat_out = wrap(out); // matrix is just a vector with dimensions (and col major...) // please don't copy data!
#ifdef ICD9_DEBUG
	std::cout << "wrapped out\n";
#endif
	mat_out.attr("dim") = Dimension((int) num_comorbid, (int) num_visits); // set dimensions in reverse (row major for parallel step)
	mat_out.attr("dimnames") = List::create(icd9Mapping.names(), out_row_names);
	Function t("t"); // use R transpose - seems pretty fast
#ifdef ICD9_DEBUG
	std::cout << "Ready to transpose and return\n";
#endif
#ifdef ICD9_VALGRIND
	CALLGRIND_STOP_INSTRUMENTATION;
	//CALLGRIND_DUMP_STATS;
#endif
	UNPROTECT(1);
	return t(mat_out);
}
