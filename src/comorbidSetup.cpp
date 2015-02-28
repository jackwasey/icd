// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include <R.h>
#include <Rinternals.h>
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

//void buildVisitCodesVec(const DataFrame& icd9df, const std::string& visitId,
//		const std::string& icd9Field, CodesVecSubtype& vcdb_n,
//		CodesVecSubtype& vcdb_v, CodesVecSubtype& vcdb_e, std::vector<int>& visitIds,
//		bool aggregate=true) {
//
//	// now I can just do long to ragged, then loop through ragged to split out the N, V and E types
//	ComorbidVecMap ragged; // mixed ICD9 codes
//	longToRagged(icd9df, ragged, visitIds, visitId, icd9Field, aggregate);
//
//	const int approx_cmb_per_visit = 15; // just an estimate
//	int vlen = ragged.size();
//	vcdb_n.reserve(vlen / approx_cmb_per_visit);
//	vcdb_v.reserve(vlen / (approx_cmb_per_visit/3)); // fewer V and E than N codes
//	vcdb_e.reserve(vlen / (approx_cmb_per_visit/3));
//
//
//	for (ComorbidVecMap::iterator vis_it = ragged.begin(); vis_it != ragged.end(); ++vis_it) {
//		// now loop through codes for one visit
//		for (VecStr::iterator codes_it = (*vis_it).begin(); codes_it != (*vis_it).end(); ++codes_it) {
//			switch((*codes_it)[0]) {
//			case 'V':
//			case 'v':
//			{
//				vcdb_v.push_back()
//
//			}
//			}
//		}
//	}
//}

// int version

// R CMD INSTALL --no-build-vignettes icd9 && R -d gdb -e "library(icd9);  mydf <- data.frame(visitId = c('a','b'), icd9=c('1','2')); icd9Comorbid(mydf, ahrqComorbid)"

void buildVisitCodesVec(const SEXP& icd9df, const std::string visitId,
		const std::string icd9Field, CodesVecSubtype& vcdb_n,
		CodesVecSubtype& vcdb_v, CodesVecSubtype& vcdb_e, std::vector<int>& visitIds,
		bool aggregate=true) {
	SEXP icds = PROTECT(getRListOrDfElement(icd9df, icd9Field.c_str()));
	SEXP vsexp = PROTECT(getRListOrDfElement(icd9df, visitId.c_str()));
	bool icd_factor = Rf_isFactor(icds);
#ifdef ICD9_DEBUG_SETUP
	if (icd_factor) std::cout << "icds are encoded as a factor\n";
#endif
	const int approx_cmb_per_visit = 15; // just an estimate
	int vlen = Rf_length(icds);
	vcdb_n.reserve(vlen / approx_cmb_per_visit);
	vcdb_v.reserve(vlen / approx_cmb_per_visit);
	vcdb_e.reserve(vlen / approx_cmb_per_visit);
	Str lastVisitId;
	switch(TYPEOF(vsexp)) {
	case INTSXP:
#ifdef ICD9_DEBUG_SETUP
		std::cout << "buildVisitCodes SEXP is INT\n";
#endif
		{
			int* vi;
			vi = INTEGER(vsexp); // point to the integer vector of visit codes
			visitIds.reserve(vlen / approx_cmb_per_visit);
			int lastVisitId = 1294967295L; // random big number
			SEXP icd_levels; // only needed if we have a factor
			if (icd_factor) {
#ifdef ICD9_DEBUG_SETUP
				if (icd_factor) std::cout << "getting levels\n";
#endif
				icd_levels = Rf_getAttrib(icds, R_LevelsSymbol);
			}
			const char* s; // length??
			for (int i = 0; i < vlen; ++i) {
#ifdef ICD9_DEBUG_SETUP_TRACE
				std::cout << "building visit: it = " << i << ", id = " << vi[i] << "\n";
				std::cout << "length vcdb_n = " << vcdb_n.size() << "\n";
#endif
				CodesVecSubtype& vcdb = vcdb_n;
				if (icd_factor) {
#ifdef ICD9_DEBUG_SETUP
					if (icd_factor) std::cout << "getting factor indices\n";
					std::cout << " type of icds = " << TYPEOF(icds) << "\n";
					std::cout << " size of icds = " << Rf_length(icds) << "\n";
#endif
					int int_temp = INTEGER(icds)[i];
#ifdef ICD9_DEBUG_SETUP
					std::cout << " factor lookup = " << int_temp << "\n";
#endif
					s = CHAR(STRING_ELT(icd_levels, int_temp-1));
				} else {
					s = CHAR(STRING_ELT(icds, i));
				}
				int n = 0;
				// would be easy to skip whitespace here too... TODO
				if (*s<'0' && *s>'9') {
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
					visitIds.push_back(vi[i]);
				}
#ifdef ICD9_DEBUG_SETUP_TRACE
				std::cout << "repeat id found: " << vi[i] << "\n";
#endif
				vcdb[vcdb.size() - 1].push_back(n); // augment vec for current visit and N/V/E type
				lastVisitId = vi[i];
			} // end loop through all visit-code input data
			break; // break out of INT SEXP switch
		}
	default:
	{
#ifdef ICD9_DEBUG_SETUP
		std::cout << "SEXP is not INT\n";
#endif

	}
	} // end switch on visitId SEXP type
#ifdef ICD9_DEBUG_SETUP
	std::cout << "visit map created\n";
#endif
	UNPROTECT(2);
}

// icd9 codes always strings, but visitId can be integer (much faster logic) or string vectors.
void buildVisitCodesVec(const SEXP& icd9df, const std::string& visitId, const std::string& icd9Field,
		CodesVecSubtype& vcdb_n, CodesVecSubtype& vcdb_v, CodesVecSubtype& vcdb_e,
		VecStr& visitIds, bool aggregate=true) {
	SEXP icds = PROTECT(getRListOrDfElement(icd9df, icd9Field.c_str()));
	SEXP vsexp = PROTECT(getRListOrDfElement(icd9df, visitId.c_str()));
	const int approx_cmb_per_visit = 15; // just an estimate
	int vlen = Rf_length(icds);
	vcdb_n.reserve(vlen / approx_cmb_per_visit);
	vcdb_v.reserve(vlen / approx_cmb_per_visit);
	vcdb_e.reserve(vlen / approx_cmb_per_visit);
	Str lastVisitId;
	switch(TYPEOF(vsexp)) {
	case STRSXP:
#ifdef ICD9_DEBUG_SETUP
		std::cout << "buildVisitCodes SEXP is STR\n";
#endif
		{
			visitIds.reserve(vlen / approx_cmb_per_visit);
			const char* lastVisitId = "JJ94967295JJ"; // random
			for (int i = 0; i < vlen; ++i) {
				const char* vi = CHAR(STRING_ELT(vsexp, i));
				const char* s  = CHAR(STRING_ELT(icds,  i));
#ifdef ICD9_DEBUG_SETUP_TRACE
				std::cout << "building visit: it = " << i << ", id = " << vi[i] << "\n";
				std::cout << "length vcdb_n = " << vcdb_n.size() << "\n";
#endif
				CodesVecSubtype& vcdb = vcdb_n;
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
				if (lastVisitId != vi && (aggregate && std::find(visitIds.rbegin(), visitIds.rend(), vi) == visitIds.rend())) {

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
					visitIds.push_back(vi);
				}
#ifdef ICD9_DEBUG_SETUP_TRACE
				std::cout << "repeat id found: " << vi[i] << "\n";
#endif
				vcdb[vcdb.size() - 1].push_back(n); // augment vec for current visit and N/V/E type
				lastVisitId = vi;
			} // end loop through all visit-code input data
			break; // break out of INT SEXP switch
		}
	default:
	{
#ifdef ICD9_DEBUG_SETUP
		std::cout << "SEXP is not STR\n";
#endif
	}
	} // end switch on visitId SEXP type
#ifdef ICD9_DEBUG_SETUP
	std::cout << "visit map created\n";
#endif
	UNPROTECT(2);
}

