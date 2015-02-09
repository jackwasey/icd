// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::depends(RcppParallel,BH)]]
#include <local.h>
#include <Rcpp.h>
#include <RcppParallel.h>

using namespace RcppParallel;
using namespace Rcpp;

struct ComorbidWorker : public Worker {
	const VecCodes vc;
	const ComorbidVecMap map;
	const ComorbidVecMap::size_type num_comorbid;
	const MapVecStr::size_type num_visits;
	VecBool out; // vector of booleans we can restructure to a data.frame later

	// constructors
	ComorbidWorker(VecCodes vc, ComorbidVecMap map)
	: vc(vc), map(map), num_comorbid(map.size()), num_visits(vc.size()),
	  out(std::vector<bool>(vc.size()*map.size(), false)) {}

	void operator()(std::size_t begin, std::size_t end) {
		// do the work for the given range

		VecCodes::const_iterator chunkbegin = vc.begin();
		VecCodes::const_iterator chunkend = vc.begin();
		advance(chunkbegin, begin);
		advance(chunkend, end);

#ifdef ICD9_DEBUG
		std::cout << "got the following number of unique visitIds: ";
		std::cout << num_visits << "\n";
		std::cout << "begin = " << begin <<", end = " << end << "\n";
#endif

		// iterate through the block of codes which we have been given
		for(VecCodes::const_iterator vis_it = chunkbegin; vis_it != chunkend; ++vis_it) {

			// find the icd9 codes for a given visitId
			const VecStr codes = *vis_it; // these are the ICD-9 codes for the current visitid

			int urow = std::distance(vc.begin(), vis_it); // TODO: prefer simple integer as distance has cost (?only when working on a set)
#ifdef ICD9_DEBUG
			std::cout << "urow = " << urow <<"\n";
#endif

			for (ComorbidVecMap::size_type cmb = 0; cmb < num_comorbid; ++cmb) {
#ifdef ICD9_TRACE
				std::cout << "working on cmb: " << cmb << "... ";
#endif
				// loop through icd codes for this visitId
				const VecStr::const_iterator cbegin = codes.begin();
				const VecStr::const_iterator cend = codes.end();
				for (VecStr::const_iterator code_it = cbegin; code_it != cend; ++code_it) {
#ifdef ICD9_TRACE
					std::cout << "working on code: " << *code_it << "\n";
					printSetStr(map[cmb]);
#endif
					if (std::binary_search(map[cmb].begin(), map[cmb].end(), *code_it)) {
						VecBool::size_type out_idx = cmb*(num_visits-1) + urow;
#ifdef ICD9_TRACE
						std::cout << "found match";
						std::cout << out.size() << ", but idx = " << out_idx << "\n";
#endif
						// no bounds check: confidence in the mathematics
						out[out_idx] = true; // and update the current item. This is where we define the matrix indexing to be by visitid first, then cmb, which fits with a dataframe of a list of columns.
						continue;
					}
#ifdef ICD9_TRACE
					std::cout << "\n";
#endif
				}
			} // end for looping through whole comorbidity map
		}
	}
};

//' @rdname icd9Comorbid
//' @description RcppParallel approach
//' @export
// [[Rcpp::export]]
List icd9ComorbidShortRcppParallelFullVector(DataFrame icd9df, List icd9Mapping,
		const std::string visitId = "visitId", const std::string icd9Field = "icd9") {

	const VecStr vs = as<VecStr>(as<CharacterVector>(icd9df[visitId]));
	const VecStr icds = as<VecStr>(as<CharacterVector>(icd9df[icd9Field]));

	//int vlen = vs.size();

	/* create vector of vectors of code lists, and a vector of corresponding visit Ids.
	 * The visit Ids do not have to be in order, but do have to be unique (or they will be repeated in comorbidity data frame)
	 * 1. get list of unique visit Ids
	 * 2. iterate through the unique visit ids
	 * 2a. find all the other visit id indexes
	 * 2b. find corresponding icd codes and push them onto a vector of vectors
	 */

	VecStr unique_visits = vs;
	std::sort(unique_visits.begin(), unique_visits.end());
	unique_visits.erase( std::unique(unique_visits.begin(), unique_visits.end()), unique_visits.end());
	VecStr::size_type unique_len = unique_visits.size();

	VecCodes vc;

	for (VecStr::iterator i=unique_visits.begin(); i!=unique_visits.end(); ++i) {
#ifdef ICD9_DEBUG
		std::cout << "building visit: unique visit = " << *i << "\n";
		std::cout << "length vc = " << vc.size() << "\n";
		std::cout << "length unique visits = " << unique_len << "\n";
#endif

		// find the range for the current icd code (the vs list is not sorted)

		const VecStr::const_iterator vfirstmatch = std::find(vs.begin(), vs.end(), *i);
#ifdef ICD9_DEBUG
		std::cout << "got vfirstmatch\n";
#endif
		const VecStr::const_iterator vlastmatch = std::upper_bound(vfirstmatch+1, vs.end(), *i)-1;
#ifdef ICD9_DEBUG
		std::cout << "got match iterators\n";
#endif
		// it may be possible to use the vs iterator directly on icds, but i'll get distance formally:
		VecStr::size_type icds_first = std::distance(vs.begin(), vfirstmatch);
		VecStr::size_type icds_last = std::distance(vs.begin(), vlastmatch);
#ifdef ICD9_DEBUG
		std::cout << "got icds indices: " << icds_first << " and " << icds_last << "\n";
		if (icds.begin()+icds_last >= icds.end()) {
			std::cout << "overflow\n";
			continue;
		}
#endif
		VecStr codes_one_visit(icds.begin()+icds_first, icds.begin()+icds_last);
#ifdef ICD9_DEBUG
		std::cout << "got icds codes with length: " << codes_one_visit.size() << "\n";
#endif
		vc.push_back(codes_one_visit);
	}
#ifdef ICD9_DEBUG
	std::cout << "visit map created\n";
#endif

	CharacterVector mapnames = icd9Mapping.names();

	// convert mapping from List of CharacterVectors to std vector of sets. This
	// is a small one-off cost, and dramatically improves the performance of the
	// later loops, because we can .find() instead of linear search.
	ComorbidVecMap map;
	for (List::iterator mi = icd9Mapping.begin(); mi != icd9Mapping.end(); ++mi) {
		VecStr mvs(as<VecStr>(*mi));
		std::sort(mvs.begin(), mvs.end());
		map.push_back(mvs);
	}
#ifdef ICD9_DEBUG
	std::cout << "reference comorbidity mapping STL structure created\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n";
#endif

	ComorbidWorker worker(vc, map);
#ifdef ICD9_DEBUG
	std::cout << "worker instantiated with size " << vc.size() << "\n&&&&&&&&&&&&&&&&&&&&&&&&&&&&&\n";
#endif
	// PARALLEL FOR HAS TO SPLIT UP WITH EACH VISITID TOGETHER: can't just slice whole list
	parallelFor(0, vc.size(), worker);
#ifdef ICD9_DEBUG
	std::cout << "work complete\n**********************************\n";
#endif

	List df_out;
	df_out[visitId] = wrap(unique_visits);
	// loop through comorbidities to extract logical vectors
#ifdef ICD9_DEBUG
	int n = (int)worker.out.size();
	std::cout << "worker.out length = " << n <<"\n";
	std::cout << "worker.num_comorbid = " << worker.num_comorbid <<"\n";
#endif

	for (size_t i=0;i<worker.num_comorbid;++i) {
		const String cmb_name = mapnames[i];
		VecBool::iterator start = worker.out.begin();
		VecBool::iterator end = worker.out.begin();
		std::advance(start, i*worker.num_visits);
		std::advance(end, (i+1)*(worker.num_visits));
#ifdef ICD9_DEBUG
		size_t dstart = std::distance(worker.out.begin(), start);
		size_t dend = std::distance(worker.out.begin(), end);
		std::cout << "dstart = " << dstart << ", dend = " << dend << "\n";
#endif
		LogicalVector lv(start, end);
		df_out[cmb_name] = lv;
	}
	IntegerVector row_names = seq_len(worker.num_visits);
	df_out.attr("row.names") = row_names;
	df_out.attr("class") = "data.frame";
	return df_out;
}
