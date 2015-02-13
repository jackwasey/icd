// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::depends(RcppParallel,BH)]]
#include <local.h>
#include <Rcpp.h>
#include <RcppParallel.h>

using namespace RcppParallel;
using namespace Rcpp;

struct ComorbidWorkerHV : public Worker {
  const MapVecStr vcdb;
  const ComorbidVecMap map;
  const CharacterVector mapnames;
  const ComorbidVecMap::size_type num_comorbid;
  const MapVecStr::size_type num_visits;
  VecBool out; // vector of booleans we can restructure to a data.frame later

  // constructors
  ComorbidWorkerHV(MapVecStr vcdb, ComorbidVecMap map, CharacterVector mapnames)
  : vcdb(vcdb), map(map), mapnames(mapnames),
  num_comorbid(map.size()), num_visits(vcdb.size()),
  out(std::vector<bool>(vcdb.size()*map.size(), false)) {}

  void operator()(std::size_t begin, std::size_t end) {
    // do the work for the given range

    MapVecStr::const_iterator chunkbegin = vcdb.begin();
    MapVecStr::const_iterator chunkend = vcdb.begin();
    advance(chunkbegin, begin);
    advance(chunkend, end);

    #ifdef ICD9_DEBUG
    std::cout << "got the following number of unique visitIds: ";
    std::cout << vcdb.size() << "\n";
    std::cout << "begin = " << begin <<", end = " << end << "\n";
    #endif

    // iterate through the block of vcdb which we have been given
    for(MapVecStr::const_iterator vis_it = chunkbegin; vis_it != chunkend; ++vis_it) {

      // find the icd9 codes for a given visitId
      const std::string key = vis_it->first;
      const VecStr codes = vis_it->second; // these are the ICD-9 codes for the current visitid

      #ifdef ICD9_DEBUG
      std::cout << "working on key: " << key <<" with codes: ";
      //printVecStr(codes);
      #endif

      // the MapVecStr::iterator has unique keys, so we can just iterate now: TODO
      // the Tmm iterator counts down whole list of (probably duplicated) visits, so we need to keep track:
      int urow = distance(vcdb.begin(), vis_it);
      #ifdef ICD9_DEBUG
      std::cout << "urow = " << urow <<"\n";
      #endif

      // instead of assuming order of keys and counting, we need to insert the key, so the loop can go parallel
      // loop through comorbidities
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
  }
};

//' @rdname icd9Comorbid
//' @description RcppParallel approach
//' @export
// [[Rcpp::export]]
List icd9ComorbidShortRPHalfVector(DataFrame icd9df, List icd9Mapping,
const std::string visitId = "visitId", const std::string icd9Field = "icd9") {
#ifdef ICD9_DEBUG
std::cout << "icd9ComorbidShortRPHalfVector\n";
#endif

  const VecStr vs = as<VecStr>(as<CharacterVector>(icd9df[visitId]));
  const VecStr icds = as<VecStr>(as<CharacterVector>(icd9df[icd9Field]));

  // TODO reserve size for map if possible for vcdb?
  MapVecStr vcdb;
  // probably a slower initial step: create map of VecStr
  // (doesn't need to be sorted, but keys must be unique. Unsorted map though is C++11)
  int vlen = vs.size();

  for (int i=0; i<vlen; ++i) {
    #ifdef ICD9_DEBUG
    std::cout << "building visit: it = " << i << ", id = " << vs[i] << "\n";
    std::cout << "length vcdb = " << vcdb.size() << "\n";
    #endif

    MapVecStr::iterator mapit = vcdb.find(vs[i]);
    if (mapit==vcdb.end()) {
      #ifdef ICD9_DEBUG
      std::cout << "first sight of key " << vs[i] << "), so just insert the code\n";
      #endif
      VecStr vcodes(1,icds[i]); // construct one element vec str
      vcdb.insert(std::make_pair(vs[i], vcodes));
    } else {
      // no guarantee of order of visitId, so if the key already exists, we need to extend it
      // already have the visitId in map, so update it (in place?!)
      #ifdef ICD9_DEBUG
      std::cout << "repeat id found: " << vs[i] << ". Got these existing codes: ";
      //printVecStr(mapit->second);
      #endif
      (mapit->second).push_back(icds[i]);
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

  CharacterVector mapnames = icd9Mapping.names();

	ComorbidVecMap map;
	for (List::iterator mi = icd9Mapping.begin(); mi != icd9Mapping.end(); ++mi) {
		VecStr mvs(as<VecStr>(*mi));
		std::sort(mvs.begin(), mvs.end());
		map.push_back(mvs);
	}
  #ifdef ICD9_DEBUG
  std::cout << "reference comorbidity mapping STL structure created\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n";
  #endif

  ComorbidWorkerHV worker(vcdb, map, mapnames);
  #ifdef ICD9_DEBUG
  std::cout << "worker instantiated with size " << vcdb.size() << "\n&&&&&&&&&&&&&&&&&&&&&&&&&&&&&\n";
  #endif
  // PARALLEL FOR HAS TO SPLIT UP WITH EACH VISITID TOGETHER: can't just slice whole list
  parallelFor(0, vcdb.size(), worker);
  #ifdef ICD9_DEBUG
  std::cout << "work complete\n**********************************\n";
  #endif

  List df_out;
  // list of visit keys
  std::vector<std::string> visitIds;
  for (MapVecStr::iterator it=vcdb.begin(); it !=vcdb.end(); ++it) {
    visitIds.push_back(it->first);
  }
  df_out[visitId] = wrap(visitIds);
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
