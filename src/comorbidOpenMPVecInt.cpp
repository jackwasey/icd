// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::depends(RcppProgress)]]
// [[Rcpp::plugins(openmp)]]
#include <Rcpp.h>
#include <progress.hpp>
#include <local.h>
#include <string>
#include <algorithm>
//#include <parallel/algorithm>

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
#ifdef _OPENMP
	std::cout << "_OPENMP is defined. Setting threads.\n";
#endif
	if (threads > 0)
		omp_set_num_threads(threads);
	std::cout << "Number of threads=" << omp_get_max_threads() << "\n";
#endif

	//TODO: convert from std::map for visit:codes to two vectors (which would now be sorted). The map was only needed so that disordered visitIds would be taken care of. We don't need to lookup visitId again.

	VecStr _visitIds; // ideally would be const
	const VecUInt allCodes = buildVisitCodes(icd9df, visitId, icd9Field, _visitIds);
	const VecStr visitIds = clone(_visitIds); // is this worth it? good practice?

	const ComorbidVecInt map = buildMap(icd9Mapping);
	const ComorbidVecInt::size_type num_comorbid = map.size();
	const MapVecInt::size_type num_visits = visitIds.size();
	VecBool out(num_comorbid * num_visits, false); // vector of booleans we can restructure to a data.frame later
	// definitely need to work in row major for best parallel execution with spatio-temporal locality. Transpose not too expensive later

#ifdef ICD9_DEBUG
	std::cout << num_visits << " visits\n";
	std::cout << num_comorbid << " is num_comorbid\n";
#endif

	//#pragma omp single nowait
	// TODO: try __gnu_parallel::for_each
#pragma omp parallel for schedule(static,50)
	for (size_t urow = 0; urow < num_visits; ++urow) {

		// find the icd9 codes for a given visitId
		//const VecUInt codes = vis_it->second;// these are the ICD-9 codes for the current visitid
		const VecUInt codes = allCodes[urow];// these are the ICD-9 codes for the current visitid

#ifdef ICD9_TRACE
		std::cout << "working on visit: " << visitIds[urow] << " with codes: ";
		printIt(codes);
#endif

		// instead of assuming order of keys and counting, we need to insert the key, so the loop can go parallel
		// loop through comorbidities
		for (ComorbidVecInt::size_type cmb = 0; cmb < num_comorbid; ++cmb) {
			// loop through icd codes for this visitId
			const VecUInt::const_iterator cbegin = codes.begin();
			const VecUInt::const_iterator cend = codes.end();
			for (VecUInt::const_iterator code_it = cbegin; code_it != cend;
					++code_it) {
				if (std::binary_search(map[cmb].begin(), map[cmb].end(), *code_it)) {
					VecBool::size_type out_idx = num_comorbid * urow + cmb;
					out[out_idx] = true;// and update the current item. This is where we define the matrix indexing to be by visitid first, then cmb, which fits with a dataframe of a list of columns.
				}
			}
		} // end for looping through whole comorbidity map
	}
	//LogicalMatrix mat_out(num_visits, num_comorbid);
#ifdef ICD9_DEBUG
	std::cout << "Ready to convert to R Matrix\n";
#endif
	LogicalVector mat_out = wrap(out); // matrix is just a vector with dimensions (and col major...) // please don't copy data!
	mat_out.attr("dim") = Dimension((int) num_comorbid, (int) num_visits); // set dimensions in reverse (row major for parallel step)
	Function t("t"); // use R transpose - seems pretty fast
	//LogicalMatrix mat_out_t = t(mat_out);
#ifdef ICD9_DEBUG
	std::cout << "Ready to transpose\n";
#endif
	mat_out = t(mat_out);

	//CharacterVector rowNames = wrap(visitIds);
	mat_out.attr("dimnames") = List::create(visitIds, R_NilValue);
	//mat_out.attr(".Dim") = IntegerVector((int)num_visits, (int)num_comorbid);

	// loop through comorbidities to extract logical vectors, and manually transpose?
	// may be better to let R transpose, as the memory access will be crazy if I'm not careful.
	const VecStr mapnames = as<VecStr>(icd9Mapping.names());

	for (size_t i = 0; i < num_comorbid; ++i) {
		const String cmb_name = mapnames[i];
		VecBool::iterator start = out.begin();
		VecBool::iterator end = out.begin();
		std::advance(start, i * num_visits); // column wise
		std::advance(end, (i + 1) * (num_visits));
#ifdef ICD9_DEBUG
		size_t dstart = std::distance(out.begin(), start);
		size_t dend = std::distance(out.begin(), end);
		std::cout << "dstart = " << dstart << ", dend = " << dend << "\n";
#endif
		LogicalVector lv(start, end);
		//mat_out[cmb_name] = lv;
	}
	return mat_out;
}
