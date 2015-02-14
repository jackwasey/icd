// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::depends(RcppParallel,BH)]]
#include <local.h>
#include <Rcpp.h>
#include <RcppParallel.h>

using namespace RcppParallel;
using namespace Rcpp;

struct ComorbidWorkerVIB: public Worker {
	const MapVecInt vcdb;
	const ComorbidVecInt map; // map of comorbidities to ICD9 codes
	const CharacterVector mapnames;
	const ComorbidVecInt::size_type num_comorbid;
	const MapVecInt::size_type num_visits;
	VecBool out; // vector of booleans we can restructure to a data.frame later.
	// The out 'matrix' must be stored in row major, even though it would be nice just to take columns for each comorbidity
	// at the end, because we need to be able to write subsets of the rows at a time when working in parallel.

	// constructors
	ComorbidWorkerVIB(MapVecInt vcdb, ComorbidVecInt map,
			CharacterVector mapnames) :
			vcdb(vcdb), map(map), mapnames(mapnames), num_comorbid(map.size()), num_visits(
					vcdb.size()) {
	}
	// out(std::vector<bool>(vcdb.size()*map.size(), false)) // predefine size of out, but then need to replace

	void operator()(std::size_t begin, std::size_t end) {
		// do the work for the given range

		MapVecInt::const_iterator chunkbegin = vcdb.begin();
		MapVecInt::const_iterator chunkend = vcdb.begin();
		advance(chunkbegin, begin);
		advance(chunkend, end);
		out.reserve(vcdb.size() * map.size());
		MapVecInt::size_type num_visits_chunk = 1 + end - begin;
		VecBool batchOut(num_comorbid * (end - begin), false); // temporary place to write to avoid cache contention.

#ifdef ICD9_DEBUG_PARALLEL
		std::cout << vcdb.size() << " visits in this chunk\n";
#ifdef ICD9_DEBUG
		std::cout << "begin = " << begin << ", end = " << end << "\n";
#endif
#endif

		// iterate through the block of vcdb which we have been given
		for (MapVecInt::const_iterator vis_it = chunkbegin; vis_it != chunkend;
				++vis_it) {

			// find the icd9 codes for a given visitId
			const VecUInt codes = vis_it->second; // these are the ICD-9 codes for the current visitid

#ifdef ICD9_DEBUG
			std::ostringstream o;
			o << "working on key: " << vis_it->first << " with codes: ";
			std::cout << o.str();
			std::cout.flush();
			printIt(codes);
#endif

			// the MapVecStr::iterator has unique keys, so we can just iterate now: TODO
			// the Tmm iterator counts down whole list of (probably duplicated) visits, so we need to keep track:
			MapVecInt::size_type urow = distance(chunkbegin, vis_it);
#ifdef ICD9_TRACE
			std::cout << "urow = " << (int)urow <<"\n";
#endif

			// instead of assuming order of keys and counting, we need to insert the key, so the loop can go parallel
			// loop through comorbidities
			for (ComorbidVecInt::size_type cmb = 0; cmb < num_comorbid; ++cmb) {
#ifdef ICD9_TRACE
				std::cout << "working on cmb: " << cmb << "... ";
				std::cout.flush();
#endif
				// loop through icd codes for this visitId
				const VecUInt::const_iterator cbegin = codes.begin();
				const VecUInt::const_iterator cend = codes.end();
				for (VecUInt::const_iterator code_it = cbegin; code_it != cend;
						++code_it) {
#ifdef ICD9_TRACE
					std::cout << "working on code: " << *code_it << "\n";
					std::cout.flush();
					printIt(map[cmb]);
#endif
					if (std::binary_search(map[cmb].begin(), map[cmb].end(),
							*code_it)) {
						VecBool::size_type out_idx =
								urow * num_visits_chunk + cmb; // + urow;
#ifdef ICD9_TRACE
										std::cout << "found match";
										std::cout << batchOut.size() << ", out idx = " << out_idx << "\n";
#endif
						// no bounds check: confidence in the mathematics
						// and update the current item. This is where we define the matrix indexing to be by visitid first, then cmb, which fits with a dataframe of a list of columns.
						//	out[out_idx] = true;
#ifdef ICD9_DEBUG
						if (out_idx > batchOut.size()) {
							char buff[100];
							sprintf(buff, "idx = %d, but size = %d\n",
									(int) out_idx, (int) batchOut.size());
							std::cout << buff;
							std::cout.flush();
							Rcpp::stop("out idx out of range");
						}
#endif

						batchOut[out_idx] = true;
					}
				}
			} // end for looping through whole comorbidity map
		} // end loop through visits
		  // write batch out to the main out.
		  // Without doing this, even if the out data is in cache, it is locked so other threads can't touch it.
		  // TODO: is this even thread safe?
#ifdef ICD9_TRACE
		std::cout << "finished a batch\n";
		printIt(batchOut);
#endif

		out.insert(out.end(), batchOut.begin(), batchOut.end());
#ifdef ICD9_TRACE
		std::cout << "out is now: ";
		printIt(out);
#endif
	} // end operator()
};

//' @rdname icd9Comorbid
//' @description RcppParallel approach
//' @export
// [[Rcpp::export]]
List icd9ComorbidShortRPVecIntBatch(DataFrame icd9df, List icd9Mapping,
		const std::string visitId = "visitId", const std::string icd9Field =
				"icd9", int grainSize = 0) {
#ifdef ICD9_DEBUG
	std::cout << "icd9ComorbidShortRPVecIntBatch\n";
#endif

	const VecStr vs = as<VecStr>(as<CharacterVector>(icd9df[visitId]));
	const VecStr icds = as<VecStr>(as<CharacterVector>(icd9df[icd9Field]));

	// TODO reserve size for map if possible for vcdb?
	MapVecInt vcdb_n;
	MapVecInt vcdb_v;
	MapVecInt vcdb_e;

	int vlen = vs.size();

	for (int i = 0; i < vlen; ++i) {
#ifdef ICD9_DEBUG_SETUP
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

		if (*s < '0' && *s > '9') { // V or E code
			if (*s == 'V' || *s == 'v') {
				whichmap = vcdb_v;
			} else {
				whichmap = vcdb_e;
			}
			++s;

		}
		while (*s >= '0' && *s <= '9') {
			n = (n * 10) + (*s - '0');
			++s;
		}

		MapVecInt::iterator mapit = whichmap.find(vs[i]);
		if (mapit == whichmap.end()) {
#ifdef ICD9_DEBUG_SETUP
			std::cout << "first sight of key " << vs[i] << "), so just insert the code\n";
#endif
			VecUInt vcodes(1, n); // construct one element vec str
			whichmap.insert(std::make_pair(vs[i], vcodes));
		} else {
			// no guarantee of order of visitId, so if the key already exists, we need to extend it
			// already have the visitId in map, so update it (in place?!)
#ifdef ICD9_DEBUG_SETUP
			std::cout << "repeat id found: " << vs[i] << ". Got these existing codes: ";
			//printVecStr(mapit->second);
#endif
			(mapit->second).push_back(n);
#ifdef ICD9_DEBUG_SETUP
			std::cout << "updated codes: ";
			//printVecStr(mapit->second);
			std::cout << "\n";
#endif
		}
	}
#ifdef ICD9_DEBUG_SETUP
	std::cout << "visit map created\n";
#endif

	CharacterVector mapnames = icd9Mapping.names();

	/* convert mapping from List of CharacterVectors to std vector of sets of unsigned ints.
	 * May be better to use vector of vector of unsigned ints, so int comparison will be so much faster than string search.
	 */

	ComorbidVecInt map_n;
	ComorbidVecInt map_v;
	ComorbidVecInt map_e;
	/*
	 * icd9Mapping is a List of string vectors
	 */
	for (List::iterator mi = icd9Mapping.begin(); mi != icd9Mapping.end();
			++mi) {

		VecStr comorbid_strings(as<VecStr>(*mi));
		VecUInt vec_n;
		VecUInt vec_v;
		VecUInt vec_e;
		for (VecStr::iterator vi = comorbid_strings.begin();
				vi != comorbid_strings.end(); ++vi) {
			VecUInt& whichvec = vec_n;
			const char *s = (*vi).c_str();
			unsigned int n = 0;
			// would be easy to skip whitespace here too, but probably no need.

			if (*s < '0' && *s > '9') { // V or E code
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
#ifdef ICD9_DEBUG_SETUP
	std::cout << "reference comorbidity mapping STL structure created\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n";
#endif

	ComorbidWorkerVIB worker(vcdb_n, map_n, mapnames);
#ifdef ICD9_DEBUG
	std::cout << "worker instantiated with size " << vcdb_n.size()
			<< "\n&&&&&&&&&&&&&&&&&&&&&&&&&&&&&\n";
#endif
	if (grainSize != 0) {
		parallelFor(0, vcdb_n.size(), worker, grainSize);
	} else {
		parallelFor(0, vcdb_n.size(), worker);
	}
#ifdef ICD9_DEBUG
	std::cout << "work complete\n**********************************\n";
#endif

	List df_out;
	// list of visit keys
	std::vector<std::string> visitIds;
	for (MapVecInt::iterator it = vcdb_n.begin(); it != vcdb_n.end(); ++it) {
		visitIds.push_back(it->first);
	}
	df_out[visitId] = wrap(visitIds);
	// loop through comorbidities to extract logical vectors
#ifdef ICD9_DEBUG
	int n = (int) worker.out.size();
	std::cout << "worker.out length = " << n << "\n";
	std::cout << "worker.num_comorbid = " << worker.num_comorbid << "\n";
#endif

	// difficult now to restructure row major vector/matrix into R data frame, but I'm pretty sure i have the best (row major) strategy for parallelization.
	for (ComorbidVecInt::size_type i = 0; i < worker.num_comorbid; ++i) {
		LogicalVector lv(worker.num_visits);
		for (MapVecInt::size_type v = 0; v < worker.num_visits; v++) {
			lv[v] = worker.out[v * worker.num_comorbid + i]; // row major to column major
		}
		String cmb_name = mapnames[i];
		df_out[cmb_name] = lv;
	}
	IntegerVector row_names = seq_len(worker.num_visits);
	df_out.attr("row.names") = row_names;
	df_out.attr("class") = "data.frame";
	return df_out;
}
