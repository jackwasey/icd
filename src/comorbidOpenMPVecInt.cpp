// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::plugins(openmp)]]

// R CMD INSTALL --no-docs icd9 && R -e "library(icd9); icd9:::runOpenMPVecInt();"
#ifdef _OPENMP // not available on clang
#include <omp.h>
#endif
#include <Rcpp.h>
#include <local.h>
#include <string>
#include <algorithm>
// enable linux performance counting
#ifdef ICD9_VALGRIND
#include <valgrind/callgrind.h>
#endif
using namespace Rcpp;

//' @rdname icd9Comorbid
//' @description RcppParallel approach with openmp and vector of integer strategy
//' @export
// [[Rcpp::export]]
SEXP icd9ComorbidShort(const DataFrame icd9df, const List icd9Mapping, const std::string visitId="visitId",
		const std::string icd9Field="icd9", const int threads=8, const size_t chunkSize=256, const size_t ompChunkSize=1) {
#ifdef ICD9_VALGRIND
	CALLGRIND_START_INSTRUMENTATION;
#endif
#if (defined ICD9_DEBUG_SETUP || defined ICD9_SETUP)
	std::cout << "icd9ComorbidShortOpenMPVecInt\n";
	std::cout << "chunk size = " << chunkSize << "\n";
#endif

#ifdef ICD9_DEBUG_PARALLEL
	std::cout << "checking _OPENMP... ";
#endif
#ifdef _OPENMP
#ifdef ICD9_DEBUG_PARALLEL
	std::cout << "_OPENMP is defined.\n";
#endif
	if (threads > 0)
		omp_set_num_threads(threads);
#ifdef ICD9_DEBUG_PARALLEL
	std::cout << "Max Number of threads=" << omp_get_max_threads() << "\n";
#endif
#endif

	VecStr _visitIds; // ideally would be const
#ifdef ICD9_DEBUG_SETUP
	std::cout << "building visit:codes structure\n";
#endif

	//const CodesVecSubtype allCodes = buildVisitCodesVecFromMap(icd9df, visitId, icd9Field, _visitIds);
	CodesVecSubtype vcdb_n;
	CodesVecSubtype vcdb_v;
	CodesVecSubtype vcdb_e;
	buildVisitCodesVec(icd9df, visitId, icd9Field, vcdb_n, vcdb_v, vcdb_e, _visitIds);

	const VecStr visitIds = _visitIds; // worth it?

#ifdef ICD9_DEBUG_SETUP
	std::cout << "building icd9Mapping\n";
#endif
	ComorbidVecInt map_n;
	ComorbidVecInt map_v;
	ComorbidVecInt map_e;
	buildMap(icd9Mapping, map_n, map_v, map_e);

#ifdef ICD9_DEBUG_SETUP
		std::cout << "first cmb has len: " << map_n[0].size() << "\n";
#endif

	const ComorbidVecInt::size_type num_comorbid = map_n.size(); // should be same for V and E
	const CodesVecSubtype::size_type num_visits = visitIds.size();

#ifdef ICD9_DEBUG_SETUP
	std::cout << num_visits << " visits\n";
	std::cout << num_comorbid << " is num_comorbid\n";
#endif

	//const Out out = lookupComorbidByRowFor(allCodes, map, chunkSize);
	//const Out out = lookupComorbidByChunkWhile(allCodes, map, chunkSize);
	const Out out = lookupComorbidByChunkFor(vcdb_n, vcdb_v, vcdb_e, map_n, map_v, map_e, chunkSize, ompChunkSize);

#ifdef ICD9_DEBUG
	std::cout << "out length is " << out.size() << "\n";
	std::cout << "Ready to convert to R Matrix\n";
#endif
#ifdef ICD9_TRACE
	std::cout << "out is: ";
	printIt(out);
#endif
	IntegerVector mat_out = wrap(out); // matrix is just a vector with dimensions (and col major...) // please don't copy data!
	//IntegerVector mat_out = wrap(out); // matrix is just a vector with dimensions (and col major...) // please don't copy data!
	mat_out.attr("dim") = Dimension((int) num_comorbid, (int) num_visits); // set dimensions in reverse (row major for parallel step)
	mat_out.attr("dimnames") = List::create(icd9Mapping.names(), visitIds);
	Function t("t"); // use R transpose - seems pretty fast
#ifdef ICD9_DEBUG
	std::cout << "Ready to transpose and return\n";
#endif
#ifdef ICD9_VALGRIND
	CALLGRIND_STOP_INSTRUMENTATION;
	CALLGRIND_DUMP_STATS;
#endif

	return t(mat_out);
}
