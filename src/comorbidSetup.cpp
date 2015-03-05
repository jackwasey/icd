// [[Rcpp::interfaces(r, cpp)]]
#include <local.h>
using namespace Rcpp;

void buildMap(const List& icd9Mapping, VecVecInt& map) {
	for (List::const_iterator mi = icd9Mapping.begin(); mi != icd9Mapping.end();
			++mi) {
		VecInt vec(as<VecInt>(*mi));
		std::sort(vec.begin(), vec.end());
#ifdef ICD9_DEBUG_SETUP_TRACE
		std::cout << "pushing back vec of length: " << vec.size() << "\n";
#endif
		map.push_back(vec);
#ifdef ICD9_DEBUG_SETUP_TRACE
		std::cout << "last vec pushed back has length: " << map[map.size()-1].size() << "\n";
#endif
	}
#ifdef ICD9_DEBUG_SETUP
	std::cout << "reference comorbidity mapping STL structure created\n";
#endif
}

// R CMD INSTALL --no-build-vignettes icd9 && R -d gdb -e "library(icd9);  mydf <- data.frame(visitId = c('a','b'), icd9=c('1','2')); icd9Comorbid(mydf, ahrqComorbid)"

// icd9 codes always strings, but visitId can be integer (much faster logic) or string vectors.
void buildVisitCodesVec(const SEXP& icd9df, const std::string& visitId,
		const std::string& icd9Field, VecVecInt& vcdb, VecStr& visitIds,
		const bool aggregate = true) {
	// TODO: ?allocate as well, or are we just getting refs here???
	SEXP icds = PROTECT(getRListOrDfElement(icd9df, icd9Field.c_str()));
	SEXP vsexp = PROTECT(getRListOrDfElement(icd9df, visitId.c_str()));
	const int approx_cmb_per_visit = 15; // just an estimate
	int vlen = Rf_length(icds);
	vcdb.reserve(vlen / approx_cmb_per_visit);
	int vcdb_idx = -1;
	if (TYPEOF(vsexp) != STRSXP) {
		stop("buildVisitCodesVec requires STRSXP");
	}
#ifdef ICD9_DEBUG_SETUP
	std::cout << "buildVisitCodes SEXP is STR\n";
#endif
	visitIds.reserve(vlen / approx_cmb_per_visit);
	const char* lastVisitId = "JJ94967295JJ"; // random
	int n;
	for (int i = 0; i < vlen; ++i) {
		const char* vi = CHAR(STRING_ELT(vsexp, i));
		n = INTEGER(icds)[i];
#ifdef ICD9_DEBUG_SETUP_TRACE
		std::cout << "building visit: it = " << i << ", id = " << vi[i] << "\n";
		std::cout << "length vcdb_n = " << vcdb_n.size() << "\n";
#endif
		// would be easy to skip whitespace here too, but probably no need.
		// with much shorter lookups with factorization of only relevant mapping codes, linear search may be better again:
		if (lastVisitId != vi
				&& (aggregate
						&& std::find(visitIds.rbegin(), visitIds.rend(), vi)
								== visitIds.rend())) {

#ifdef ICD9_DEBUG_SETUP_TRACE
			std::cout << "new key " << vi << "\n";
#endif

			VecInt vcodes;
			vcodes.reserve(approx_cmb_per_visit);
			vcodes.push_back(n);
			vcdb.push_back(vcodes);
			visitIds.push_back(vi);
			++vcdb_idx; // save us looking up current size every time when we hit a repeat.
		}
#ifdef ICD9_DEBUG_SETUP_TRACE
		std::cout << "repeat id found: " << vi[i] << "\n";
#endif
		vcdb[vcdb_idx].push_back(n); // augment vec for current visit and N/V/E type
		lastVisitId = vi;
	} // end loop through all visit-code input data
#ifdef ICD9_DEBUG_SETUP
	std::cout << "visit map created\n";
#endif
	UNPROTECT(2);
}

