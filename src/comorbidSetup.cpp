// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include <local.h>
#include <string>
#include <algorithm>
using namespace Rcpp;

void buildMap(const List& icd9Mapping, ComorbidVecInt& map_n, ComorbidVecInt& map_v, ComorbidVecInt& map_e) {
	for (List::const_iterator mi = icd9Mapping.begin(); mi != icd9Mapping.end();
			++mi) {
		VecStr comorbid_strings(as<VecStr>(*mi));
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

void buildVisitCodesVec(const DataFrame& icd9df, const std::string& visitId, const std::string& icd9Field,
		CodesVecSubtype& vcdb_n, CodesVecSubtype& vcdb_v, CodesVecSubtype& vcdb_e, VecStr& visitIds) {
	const VecStr vs = as<VecStr>(as<CharacterVector>(icd9df[visitId])); // ?unavoidable fairly slow step for big n
	const VecStr icds = as<VecStr>(as<CharacterVector>(icd9df[icd9Field]));
	const unsigned int approx_cmb_per_visit = 5; // just an estimate
	VecStr::size_type vlen = vs.size();
	vcdb_n.reserve(vlen/approx_cmb_per_visit);
	vcdb_v.reserve(vlen/approx_cmb_per_visit);
	vcdb_e.reserve(vlen/approx_cmb_per_visit);
	Str last_visit;
	for (VecStr::size_type i = 0; i < vlen; ++i) {
#ifdef ICD9_DEBUG_SETUP_TRACE
		std::cout << "building visit: it = " << i << ", id = " << vs[i] << "\n";
		std::cout << "length vcdb_n = " << vcdb_n.size() << "\n";
#endif
		/*
		 * see if code is numeric, V or E
		 * convert integer part to unsigned int
		 * add that int to the N, V or E map
		 */
		CodesVecSubtype& codeVecSubtype = vcdb_n;
		const char* s = icds[i].c_str();
		unsigned int n = 0;
		// would be easy to skip whitespace here too, but probably no need.
		if (*s < '0' && *s > '9') {
			// V or E code
			if (*s == 'V' || *s == 'v') {
				codeVecSubtype = vcdb_v;
			} else {
				codeVecSubtype = vcdb_e;
			}
			++s;
		}
		while (*s >= '0' && *s <= '9') {
			n = (n * 10) + (*s - '0');
			++s;
		}
		// CodesVecSubtype::iterator mapit = codeVecSubtype.find(vs[i]); don't find in a vector, just see if we differ from previous
		if (vs[i] != last_visit) {
#ifdef ICD9_DEBUG_SETUP_TRACE
			std::cout << "new key " << vs[i] << "\n";
#endif

			Codes vcodes;
			vcodes.reserve(approx_cmb_per_visit); // estimate of number of codes per patient.
			// start with empty N, V and E vectors for each new patient ('new' in sequential sense from input data)
			vcdb_n.push_back(vcodes);
			vcdb_v.push_back(vcodes);
			vcdb_e.push_back(vcodes);
			visitIds.push_back(vs[i]);
		}
#ifdef ICD9_DEBUG_SETUP_TRACE
			std::cout << "repeat id found: " << vs[i] << "\n";
#endif
			codeVecSubtype[codeVecSubtype.size()-1].push_back(n); // augment vec for current visit and N/V/E type
			last_visit = vs[i];
	} // end loop through all visit-code input data
#ifdef ICD9_DEBUG_SETUP
	std::cout << "visit map created\n";
#endif
}
