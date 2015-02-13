// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::depends(RcppProgress)]]
// [[Rcpp::plugins(openmp)]]
#include <Rcpp.h>
#include <progress.hpp>
#include <local.h>
#include <string>

#ifdef _OPENMP // not available on clang
#include <omp.h>
#endif

#ifdef _OPENMP
#ifdef ICD9_DEBUG_PARALLEL
REprintf("_OPENMP is defined. Setting threads.\n");
if (threads > 0)
	omp_set_num_threads(threads);
REprintf("Number of threads=%i\n", omp_get_max_threads());
#endif
#endif

using namespace Rcpp;


//' @rdname icd9Comorbid
//' @description RcppParallel approach with openmp and int strategy
//' @export
// [[Rcpp::export]]
LogicalMatrix icd9ComorbidShortOpenMPVecInt(DataFrame icd9df, List icd9Mapping,
		const std::string visitId = "visitId", const std::string icd9Field = "icd9",
		size_t grainSize = 0) {
#ifdef ICD9_DEBUG
	std::cout << "icd9ComorbidShortRPVecInt\n";
#endif

	const VecStr vs = as<VecStr>(as<CharacterVector>(icd9df[visitId]));
	const VecStr icds = as<VecStr>(as<CharacterVector>(icd9df[icd9Field]));

	// TODO reserve size for map if possible for vcdb?
	MapVecInt vcdb_n;
	MapVecInt vcdb_v;
	MapVecInt vcdb_e;

	int vlen = vs.size();

	for (int i=0; i<vlen; ++i) {
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
		const char *s = icds[i].c_str();
		unsigned int n = 0;
		// would be easy to skip whitespace here too, but probably no need.

		if (*s<'0' && *s>'9') { // V or E code
			if (*s == 'V' || *s == 'v') {
				whichmap = vcdb_v;
			} else {
				whichmap = vcdb_e;
			}
			++s;

		}
		while (*s>='0' && *s<='9') {
			n=(n*10)+(*s-'0');
			++s;
		}

		MapVecInt::iterator mapit = whichmap.find(vs[i]);
		if (mapit==whichmap.end()) {
#ifdef ICD9_DEBUG
			std::cout << "first sight of key " << vs[i] << "), so just insert the code\n";
#endif
			VecUInt vcodes(1,n); // construct one element vec str
			whichmap.insert(std::make_pair(vs[i], vcodes));
		} else {
			// no guarantee of order of visitId, so if the key already exists, we need to extend it
			// already have the visitId in map, so update it (in place?!)
#ifdef ICD9_DEBUG
			std::cout << "repeat id found: " << vs[i] << ". Got these existing codes: ";
			//printVecStr(mapit->second);
#endif
			(mapit->second).push_back(n);
#ifdef ICD9_DEBUG
			std::cout << "updated codes: ";
			//printVecStr(mapit->second);
			std::cout << "\n";
#endif
		}
	}
#ifdef ICD9_DEBUG
	std::cout << "visit map created\n";
#endif

	const VecStr mapnames = as<VecStr>(icd9Mapping.names());

	//TODO: consider converting from std::map for visit:codes to two vectors (which would now be sorted). The map was only needed so that disordered visitIds would be taken care of. We don't need to lookup visitId again.

	/* convert mapping from List of CharacterVectors to std vector of sets of unsigned ints.
	 * May be better to use vector of vector of unsigned ints, so int comparison will be so much faster than string search.
	 */

	ComorbidVecInt map_n;
	ComorbidVecInt map_v;
	ComorbidVecInt map_e;
	/*
	 * icd9Mapping is a List of string vectors
	 */
	for (List::iterator mi = icd9Mapping.begin(); mi != icd9Mapping.end(); ++mi) {

		VecStr comorbid_strings(as<VecStr>(*mi));
		VecUInt vec_n;
		VecUInt vec_v;
		VecUInt vec_e;
		for (VecStr::iterator vi = comorbid_strings.begin(); vi != comorbid_strings.end(); ++vi) {
			VecUInt& whichvec = vec_n;
			const char *s = (*vi).c_str();
			unsigned int n = 0;
			// would be easy to skip whitespace here too, but probably no need.

			if (*s<'0' && *s>'9') { // V or E code
				if (*s == 'V' || *s == 'v') {
					whichvec = vec_v;
				} else {
					whichvec = vec_e;
				}
				++s;

			}
			while (*s>='0' && *s<='9') {
				n=(n*10)+(*s-'0');
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
#ifdef ICD9_DEBUG
	std::cout << "reference comorbidity mapping STL structure created\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n";
#endif

	// #pragma omp parallel for schedule(static) // dynamic may be better suited than static.

	const MapVecInt vcdb;
	const ComorbidVecInt map; // map of comorbidities to ICD9 codes
	const ComorbidVecInt::size_type num_comorbid = map.size();
	const MapVecInt::size_type num_visits = vcdb_n.size();
	VecBool out; // vector of booleans we can restructure to a data.frame later

	const MapVecInt::const_iterator chunkbegin = vcdb.begin();
	const MapVecInt::const_iterator chunkend = vcdb.begin();

#ifdef ICD9_DEBUG
	std::cout << vcdb.size() << " visits\n";
#endif

	// iterate through the block of vcdb which we have been given
	for(MapVecInt::const_iterator vis_it = chunkbegin; vis_it != chunkend; ++vis_it) {

		// find the icd9 codes for a given visitId
		const std::string key = vis_it->first;
		const VecUInt codes = vis_it->second; // these are the ICD-9 codes for the current visitid

#ifdef ICD9_DEBUG
		std::cout << "working on key: " << key <<" with codes: ";
		//printVecStr(codes);
#endif

		int urow = distance(vcdb.begin(), vis_it);
#ifdef ICD9_DEBUG
		std::cout << "urow = " << urow <<"\n";
#endif

		// instead of assuming order of keys and counting, we need to insert the key, so the loop can go parallel
		// loop through comorbidities
		for (ComorbidVecInt::size_type cmb = 0; cmb < num_comorbid; ++cmb) {
#ifdef ICD9_TRACE
			std::cout << "working on cmb: " << cmb << "... ";
#endif
			// loop through icd codes for this visitId
			const VecUInt::const_iterator cbegin = codes.begin();
			const VecUInt::const_iterator cend = codes.end();
			for (VecUInt::const_iterator code_it = cbegin; code_it != cend; ++code_it) {
#ifdef ICD9_TRACE
				std::cout << "working on code: " << *code_it << "\n";
				printIt(map[cmb]);
#endif
				if (std::binary_search(map[cmb].begin(), map[cmb].end(), *code_it)) {
					VecBool::size_type out_idx = cmb*(num_visits-1) + urow;
#ifdef ICD9_TRACE
					std::cout << "found match";
					std::cout << out.size() << ", but idx = " << out_idx << "\n";
#endif
					// no bounds check: confidence in the mathematics
					out[out_idx] = true; // and update the current item. This is where we define the matrix indexing to be by visitid first, then cmb, which fits with a dataframe of a list of columns.
				}
#ifdef ICD9_TRACE
				std::cout << "\n";
#endif
			}
		} // end for looping through whole comorbidity map
	}
	LogicalMatrix mat_out;
	// list of visit keys
	std::vector<std::string> visitIds;
	for (MapVecInt::iterator it=vcdb_n.begin(); it !=vcdb_n.end(); ++it) {
		visitIds.push_back(it->first);
	}
	CharacterVector rowNames = wrap(visitIds);
	mat_out.attr(".Dimnames") = List::create(rowNames, R_NilValue);
	mat_out.attr(".Dim") = IntegerVector(num_visits, num_comorbid);

	// loop through comorbidities to extract logical vectors, and manually transpose?
	// may be better to let R transpose, as the memory access will be crazy if I'm not careful.

	for (size_t i=0;i<num_comorbid;++i) {
		const String cmb_name = mapnames[i];
		VecBool::iterator start = out.begin();
		VecBool::iterator end = out.begin();
		std::advance(start, i*num_visits); // column wise
		std::advance(end, (i+1)*(num_visits));
#ifdef ICD9_DEBUG
		size_t dstart = std::distance(out.begin(), start);
		size_t dend = std::distance(out.begin(), end);
		std::cout << "dstart = " << dstart << ", dend = " << dend << "\n";
#endif
		LogicalVector lv(start, end);
		df_out[cmb_name] = lv;
	}
	IntegerVector row_names = seq_len(num_visits);
	df_out.attr("row.names") = row_names;
	df_out.attr("class") = "data.frame";
	return df_out;
}
