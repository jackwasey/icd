// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::plugins(openmp)]]
#include <Rcpp.h>
#include <local.h>
#include <string>
#include <algorithm>

#ifdef _OPENMP // not available on clang
#include <omp.h>
#endif

using namespace Rcpp;


//' @rdname icd9Comorbid
//' @description RcppParallel approach with openmp and int strategy
//' @export
// [[Rcpp::export]]
SEXP icd9ComorbidShortOpenMPVecInt(DataFrame icd9df, List icd9Mapping,
		const std::string visitId = "visitId", const std::string icd9Field =
				"icd9", int threads = 6) {
#ifdef ICD9_DEBUG
	std::cout << "icd9ComorbidShortOpenMPVecInt\n";
#endif

#ifdef ICD9_DEBUG_PARALLEL
	std::cout << "checking _OPENMP... ";
#endif
#ifdef _OPENMP
#ifdef ICD9_DEBUG_PARALLEL
	std::cout << "_OPENMP is defined. Setting threads.\n";
#endif
	if (threads > 0)
		omp_set_num_threads(threads);
#ifdef ICD9_DEBUG_PARALLEL
	std::cout << "Number of threads=" << omp_get_max_threads() << "\n";
#endif
#endif


	VecStr _visitIds; // ideally would be const
	const CodesVecSubtype allCodes = buildVisitCodes(icd9df, visitId, icd9Field, _visitIds);
	const VecStr visitIds = _visitIds; // worth it?

	const ComorbidVecInt map = buildMap(icd9Mapping);
	// todo: move these out? DOn't want to repeat them every iteration, but should be okay not being here at top level, apart from debug.
	const ComorbidVecInt::size_type num_comorbid = map.size();
	const MapVecInt::size_type num_visits = visitIds.size();

#ifdef ICD9_DEBUG
	std::cout << num_visits << " visits\n";
	std::cout << num_comorbid << " is num_comorbid\n";
#endif

	//VecBool out;
	//out.reserve(num_comorbid * num_visits); // vector of booleans we can restructure to a data.frame later
	const VecBool out = lookupComorbidByChunk(allCodes, map, 50);

#ifdef ICD9_DEBUG
	std::cout << "out length is " << out.size() << "\n";
	std::cout << "Ready to convert to R Matrix\n";
#endif
#ifdef ICD9_TRACE
	std::cout << "out is: ";
	printIt(out);
#endif
	LogicalVector mat_out = wrap(out); // matrix is just a vector with dimensions (and col major...) // please don't copy data!
	mat_out.attr("dim") = Dimension((int) num_comorbid, (int) num_visits); // set dimensions in reverse (row major for parallel step)
	mat_out.attr("dimnames") = List::create(icd9Mapping.names(), visitIds);
	Function t("t"); // use R transpose - seems pretty fast
	//LogicalMatrix mat_out_t = t(mat_out);
#ifdef ICD9_DEBUG
	std::cout << "Ready to transpose\n";
#endif
	return t(mat_out);
}
