// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include <local.h>
#include <string>
#include <algorithm>
using namespace Rcpp;

/*
 * Build vector of vector of unsigned ints to contain comorbidity map
 */
void buildMap(List icd9Mapping, ComorbidVecInt& map_v, ComorbidVecInt& map_e,
  	ComorbidVecInt& map_n) {
	for (List::iterator mi = icd9Mapping.begin(); mi != icd9Mapping.end();
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
		sort(vec_n.begin(), vec_n.end());
		sort(vec_v.begin(), vec_v.end());
		sort(vec_e.begin(), vec_e.end());
		map_n.push_back(vec_n);
		map_v.push_back(vec_v);
		map_e.push_back(vec_e);
	}
	std::cout << "reference comorbidity mapping STL structure created\n";
}

ComorbidVecInt buildMap(List icd9Mapping) {
	ComorbidVecInt map;
	return map;
}

void buildVisitCodes(const DataFrame& icd9df, const std::string& visitId,
		const std::string& icd9Field, MapVecInt& vcdb_n) {
	// const MapVecInt& vcdb_v //TODO
	// const MapVecInt& vcdb_e
	const VecStr vs = as<VecStr>(as<CharacterVector>(icd9df[visitId]));
	const VecStr icds = as<VecStr>(as<CharacterVector>(icd9df[icd9Field]));
	int vlen = vs.size();
	for (int i = 0; i < vlen; ++i) {
#ifdef ICD9_DEBUG
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
#ifdef ICD9_DEBUG
			std::cout << "first sight of key " << vs[i] << "\n";
#endif

			VecUInt vcodes(1, n); // construct one element vec str
			whichmap.insert(std::make_pair(vs[i], vcodes));
		} else {
#ifdef ICD9_DEBUG
			std::cout << "repeat id found: " << vs[i] << "\n";
#endif
			(mapit->second).push_back(n);
		}
	}
	std::cout << "visit map created\n";
}

VecUInt buildVisitCodes(const DataFrame& icd9df, const std::string& visitId,
		const std::string& icd9Field, VecStr& visitIds) {
	VecUInt& codes;
	return codes;
}
