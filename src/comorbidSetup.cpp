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
		VecUInt vec_n;
		VecUInt vec_v;
		VecUInt vec_e;
		for (VecStr::iterator vi = comorbid_strings.begin();
				vi != comorbid_strings.end(); ++vi) {
			VecUInt& whichvec = vec_n;
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

ComorbidVecInt buildMap(const List& icd9Mapping) {
	ComorbidVecInt map_n;
	ComorbidVecInt map_v;
	ComorbidVecInt map_e;

	buildMap(icd9Mapping, map_n, map_v, map_e);

#ifdef ICD9_DEBUG_SETUP
		std::cout << "first cmb has len: " << map_n[0].size() << "\n";
#endif

	return map_n;
}

void buildVisitCodesMap(const DataFrame& icd9df, const std::string& visitId,
		const std::string& icd9Field, MapVecInt& vcdb_n) {
	// const MapVecInt& vcdb_v //TODO
	// const MapVecInt& vcdb_e
	const VecStr vs = as<VecStr>(as<CharacterVector>(icd9df[visitId]));
	const VecStr icds = as<VecStr>(as<CharacterVector>(icd9df[icd9Field]));
	int vlen = vs.size();
	for (int i = 0; i < vlen; ++i) {
#ifdef ICD9_DEBUG_SETUP_TRACE
		std::cout << "building visit: it = " << i << ", id = " << vs[i] << "\n";
		std::cout << "length vcdb_n = " << vcdb_n.size() << "\n";
#endif
		/*
		 * see if code is numeric, V or E
		 * convert integer part to unsigned int
		 * add that int to the N, V or E map
		 */
		MapVecInt& whichmap = vcdb_n;
		const char* s = icds[i].c_str();
		unsigned int n = 0;
		// would be easy to skip whitespace here too, but probably no need.
		if (*s < '0' && *s > '9') {
			// V or E code
			if (*s == 'V' || *s == 'v') {
				//whichmap = vcdb_v;
			} else {
				//whichmap = vcdb_e;
			}
			++s;
		}
		while (*s >= '0' && *s <= '9') {
			n = (n * 10) + (*s - '0');
			++s;
		}
		MapVecInt::iterator mapit = whichmap.find(vs[i]);
		if (mapit == whichmap.end()) {
#ifdef ICD9_DEBUG_SETUP_TRACE
			std::cout << "first sight of key " << vs[i] << "\n";
#endif

			VecUInt vcodes(1, n); // construct one element vec str
			whichmap.insert(std::make_pair(vs[i], vcodes));
		} else {
#ifdef ICD9_DEBUG_SETUP_TRACE
			std::cout << "repeat id found: " << vs[i] << "\n";
#endif
			(mapit->second).push_back(n);
		}
	}
#ifdef ICD9_DEBUG_SETUP
	std::cout << "visit map created\n";
#endif
}

void buildVisitCodesVec(const DataFrame& icd9df, const std::string& visitId, const std::string& icd9Field,
		CodesVecSubtype& vcdb_n, CodesVecSubtype& vcdb_v, CodesVecSubtype& vcdb_e, VecStr& visitIds) {
	const VecStr vs = as<VecStr>(as<CharacterVector>(icd9df[visitId]));
	const VecStr icds = as<VecStr>(as<CharacterVector>(icd9df[icd9Field]));
	const unsigned int cmb_per_visit = 5;
	int vlen = vs.size();
	vcdb_n.reserve(vlen/cmb_per_visit);
	vcdb_v.reserve(vlen/cmb_per_visit);
	vcdb_e.reserve(vlen/cmb_per_visit);
	Str last_visit;
	for (int i = 0; i < vlen; ++i) {
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

			VecUInt vcodes;
			vcodes.reserve(5); // estimate of number of codes per patient.
			vcodes.push_back(n); // construct one element vec str
			codeVecSubtype.push_back(vcodes);
			visitIds.push_back(vs[i]);
		} else {
#ifdef ICD9_DEBUG_SETUP_TRACE
			std::cout << "repeat id found: " << vs[i] << "\n";
#endif
			codeVecSubtype[codeVecSubtype.size()-1].push_back(n); // augment current visit
		}
	}
#ifdef ICD9_DEBUG_SETUP
	std::cout << "visit map created\n";
#endif
}

CodesVecSubtype buildVisitCodesVecFromMap(const DataFrame& icd9df, const std::string& visitId,
		const std::string& icd9Field, VecStr& visitIds) {
#ifdef ICD9_DEBUG_SETUP
			std::cout << "running build visit codes, with return val\n";
#endif
	CodesVecSubtype codes;
	MapVecInt temp_map;
	codes.reserve(temp_map.size()); // doesn't make much difference in speed
	buildVisitCodesMap(icd9df, visitId, icd9Field, temp_map); //this is the slow step
#ifdef ICD9_DEBUG_SETUP
			std::cout << "push back map results to vector\n";
#endif
	for (MapVecInt::iterator it = temp_map.begin(); it != temp_map.end(); ++it) {
		visitIds.push_back(it->first);
		codes.push_back(it->second);

	}
	return codes; //TODO: return codes in seq we use? May be obsolete if I keep input order.
}

