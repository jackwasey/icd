// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::plugins(openmp)]]
#include <local.h>
using namespace Rcpp;

// R CMD INSTALL --no-docs icd9 && R -e "library(icd9); icd9:::runOpenMPVecInt();"

//' @rdname icd9Comorbid
//' @description RcppParallel approach with openmp and vector of integer strategy
//' @param aggregate single logical value, if /code{TRUE}, then take (possible much) more time to aggregate out-of-sequence visit IDs in the icd9df data.frame. If this is \code{FALSE}, then each contiguous group of visit IDs will result in a row of comorbidities in the output data. If you know your visitIds are possible disordered, then use \code{TRUE}.
//' @keywords internal
// [[Rcpp::export]]
SEXP icd9ComorbidShortCpp(const SEXP& icd9df, const List& icd9Mapping,
		const std::string visitId = "visitId", const std::string icd9Field =
				"icd9", const int threads = 8, const int chunkSize = 256,
		const int ompChunkSize = 1, bool aggregate = true) {
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

	VecVecInt vcdb; //size is reserved later
	// TODO: do I need to allocate memory when I do this?
	SEXP vsexp = PROTECT(getRListOrDfElement(icd9df, visitId.c_str()));
#ifdef ICD9_DEBUG_SETUP
	std::cout << "type of vsexp = " << TYPEOF(vsexp) << "\n";
#endif
	if (TYPEOF(vsexp) != STRSXP)
		Rcpp::stop("expecting vsexp to be character vector");

#ifdef ICD9_DEBUG_SETUP
	std::cout << "icd9ComorbidShortMatrix STRSXP\n";
#endif
	buildVisitCodesVec(icd9df, visitId, icd9Field, vcdb, out_row_names,
			aggregate);

#ifdef ICD9_DEBUG_SETUP
	std::cout << "building icd9Mapping\n";
#endif
	VecVecInt map;
	buildMap(icd9Mapping, map);

#ifdef ICD9_DEBUG_SETUP
	std::cout << "first cmb has len: " << map[0].size() << "\n";
#endif

	const VecVecInt::size_type num_comorbid = map.size(); // should be same for V and E
	//const CodesVecSubtype::size_type num_visits = visitIds.size();
	const VecVecIntSz num_visits = vcdb.size();

#ifdef ICD9_DEBUG_SETUP
	std::cout << num_visits << " visits\n";
	std::cout << num_comorbid << " is num_comorbid\n";
#endif

	//const Out out = lookupComorbidByRowFor(allCodes, map, chunkSize);
	//const Out out = lookupComorbidByChunkWhile(allCodes, map, chunkSize);
	const ComorbidOut out = lookupComorbidByChunkFor(vcdb, map, chunkSize,
			ompChunkSize);

#ifdef ICD9_DEBUG
	std::cout << "out length is " << out.size() << "\n";
	int outsum = std::accumulate(out.begin(), out.end(), 0);
	std::cout << "out sum is " << outsum << "\n";
	std::cout << "Ready to convert to R Matrix\n";
#endif
#ifdef ICD9_DEBUG_TRACE
	std::cout << "out is: ";
	printIt(out);
	std::cout << "printed\n";
#endif
	//IntegerVector mat_out = wrap(out); // matrix is just a vector with dimensions (and col major...) // please don't copy data!
	// TODO: the above line segfaults consistently with some input, e.g. 2e6 rows on linux. Need to manually convert int to logical?

	// try cast to logical first. (in which case I can use char for Out)
	std::vector<bool> intermed;
	intermed.assign(out.begin(), out.end());
#ifdef ICD9_DEBUG
	std::cout << "static_cast to vec bool completed\n";
#endif
	LogicalVector mat_out = wrap(intermed); // matrix is just a vector with dimensions (and col major...) // please don't copy data!
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
