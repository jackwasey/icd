// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include <local.h>
#include <string>
#include <algorithm>
extern "C" {
#include "local_c.h"
}
using namespace Rcpp;

void buildMap(const List& icd9Mapping, ComorbidVecInt& map_n,
		ComorbidVecInt& map_v, ComorbidVecInt& map_e) {
	for (List::const_iterator mi = icd9Mapping.begin(); mi != icd9Mapping.end(); ++mi) {
		VecStr comorbid_strings(as<VecStr>(*mi));
		//SEXP comorbid_strings = PROTECT(getRListOrDfElement()))
		Codes vec_n;
		Codes vec_v;
		Codes vec_e;
		for (VecStr::iterator vi = comorbid_strings.begin();
				vi != comorbid_strings.end(); ++vi) {
			Codes& whichvec = vec_n;
			const char* s = (*vi).c_str();
			unsigned int n = 0;
			// would be easy to skip whitespace here too, but probably no need.
			if (*s < '0' && *s > '9') {
				// V or E code
				if (*s == 'V' || *s == 'v') {
					whichvec = vec_v;
				} else {
					whichvec = vec_e;
				}
				++s;
			}
			while (*s >= '0' && *s <= '9') {
				n = (n * 10) + (*s - '0');
				++s;
			}
			// push the int code to the right set:
			whichvec.push_back(n);
		}
		std::sort(vec_n.begin(), vec_n.end());
		std::sort(vec_v.begin(), vec_v.end());
		std::sort(vec_e.begin(), vec_e.end());
#ifdef ICD9_DEBUG_SETUP_TRACE
		std::cout << "pushing back vec of length: " << vec_n.size() << "\n";
#endif
		map_n.push_back(vec_n);
#ifdef ICD9_DEBUG_SETUP_TRACE
		std::cout << "last vec pushed back has length: " << map_n[map_n.size()-1].size() << "\n";
#endif
		map_v.push_back(vec_v);
		map_e.push_back(vec_e);
	}
#ifdef ICD9_DEBUG_SETUP
	std::cout << "reference comorbidity mapping STL structure created\n";
#endif
}

// icd9 codes always strings, but visitId can be integer (much faster logic) or string vectors.
template <typename VISID>
void buildVisitCodesVec(const DataFrame& icd9df, const std::string& visitId,
		const std::string& icd9Field, CodesVecSubtype& vcdb_n,
		CodesVecSubtype& vcdb_v, CodesVecSubtype& vcdb_e, std::vector<VISID>& visitIds,
		bool aggregate=true) {
	SEXP icds = PROTECT(getRListOrDfElement(icd9df, icd9Field.c_str()));
	SEXP vsexp = PROTECT(getRListOrDfElement(icd9df, visitId.c_str()));
	const int approx_cmb_per_visit = 15; // just an estimate
	int vlen = Rf_length(icds);
	vcdb_n.reserve(vlen / approx_cmb_per_visit);
	vcdb_v.reserve(vlen / approx_cmb_per_visit);
	vcdb_e.reserve(vlen / approx_cmb_per_visit);
	Str lastVisitId;
	switch(TYPEOF(vsexp)) {
	case INTSXP:
#ifdef ICD9_DEBUG_SETUP
		std::cout << "SEXP is INT\n";
#endif
		{
			int* vi;
			vi = INTEGER(vsexp); // point to the integer vector of visit codes
			VISID visitIdsInt(0); // initialize as empty, don't just define?
			visitIdsInt.reserve(vlen / approx_cmb_per_visit);
			int lastVisitId = 1294967295L; // random big number
			for (int i = 0; i < vlen; ++i) {
#ifdef ICD9_DEBUG_SETUP_TRACE
				std::cout << "building visit: it = " << i << ", id = " << vs[i] << "\n";
				std::cout << "length vcdb_n = " << vcdb_n.size() << "\n";
#endif
				CodesVecSubtype& vcdb = vcdb_n;
				const char* s = CHAR(STRING_ELT(icds, i));
				int n = 0;
				// would be easy to skip whitespace here too, but probably no need.
				if (*s<'0' && *s>'9') {
					// V or E code
					if (*s == 'V' || *s == 'v')
						vcdb = vcdb_v;
					else
						vcdb = vcdb_e;
					++s;
				}
				while (*s>='0' && *s<='9') {
					n = (n*10)+(*s-'0');
					++s;
				}
				if (lastVisitId != vi[i] && (aggregate && std::find(visitIds.rbegin(), visitIds.rend(), vi[i]) == visitIds.rend())) {

#ifdef ICD9_DEBUG_SETUP_TRACE
					std::cout << "new key " << vi << "\n";
#endif

					Codes vcodes;
					vcodes.reserve(approx_cmb_per_visit); // estimate of number of codes per patient.
					// start with empty N, V and E vectors for each new patient ('new' in sequential sense from input data)
					vcdb_n.push_back(vcodes);
					vcdb_v.push_back(vcodes);
					vcdb_e.push_back(vcodes);
					//visitIds.push_back(vi);
					//visitIds.push_back(myuitos(visitIdsInt[j])); // v fast conversion (if needed)
					visitIds.push_back(visitIdsInt[i]);
				}
#ifdef ICD9_DEBUG_SETUP_TRACE
				std::cout << "repeat id found: " << vs[i] << "\n";
#endif
				vcdb[vcdb.size() - 1].push_back(n); // augment vec for current visit and N/V/E type
				lastVisitId = vi;
			} // end loop through all visit-code input data
			break; // break out of INT SEXP switch
		}
	case STRSXP:
	{
#ifdef ICD9_DEBUG_SETUP
		std::cout << "SEXP is STR\n";
#endif

	}
	} // end switch on visitId SEXP type
#ifdef ICD9_DEBUG_SETUP
	std::cout << "visit map created\n";
#endif
	UNPROTECT(2);
}
