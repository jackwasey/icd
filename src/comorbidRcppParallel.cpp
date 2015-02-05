// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <local.h>
#include <string>
#include <RcppParallel.h>

using namespace Rcpp;

struct ComorbidWorker : public Worker {
  MapVecStr vcdb;
  CmbMap map;
  CharacterVector mapnames;
  int num_comorbid;
  VB out; // vector of bools we can restructure to a data.frame later

  // constructors
  ComorbidWorker(MapVecStr vcdb, CmbMap map, CharacterVector mapnames)
  : vcdb(vcdb), map(map), mapnames(mapnames), out(std::vector<bool>(vcdb.size()*map.size(), false)) {}

  void operator()(std::size_t begin, std::size_t end) {
    // do the work for the given range

    MapVecStr::iterator chunkbegin = vcdb.begin();
    MapVecStr::iterator chunkend = vcdb.begin();
    advance(chunkbegin, begin);
    advance(chunkend, end-1);

    #ifdef ICD9_DEBUG
    std::cout << "got the following unique visitIds: ";
    std::cout << vcdb.size() << "\n";
    #endif

    num_comorbid = map.size(); // the number of comorbidity groups

    // iterate through the block of vcdb which we have been given
    for(MapVecStr::iterator vis_it = chunkbegin; vis_it != chunkend; ++vis_it) {

      // find the icd9 codes for a given visitId
      std::string key = vis_it->first;
      VecStr codes = vis_it->second; // these are the ICD-9 codes for the current visitid

      #ifdef ICD9_DEBUG
      std::cout << "working on key: " << key << "\n";
      printVecStr(codes);
      #endif

// the MapVecStr::iterator has unique keys, so we can just iterate now: TODO
      // the Tmm iterator counts down whole list of (probably duplicated) visits, so we need to keep track:
      int urow = distance(vcdb.begin(), vis_it);

      // instead of assuming order of keys and counting, we need to insert the key, so the loop can go parallel
      // loop through comorbidities
      for (int cmb = 0; cmb < num_comorbid; ++cmb) {
        // loop through icd codes for this visitId
        for (VecStrIt code_it = codes.begin(); code_it != codes.end(); ++code_it) {
          if (map[cmb].find(*code_it) != map[cmb].end()) {
            out[cmb*(num_comorbid-1) + urow] = true; // and update the current 'row'
          }
        }
      } // end for looping through whole comorbidity map
    }
  }
};

//' @rdname icd9Comorbid
//' @description RcppParallel approach
//' @export
// [[Rcpp::export]]
std::vector<bool> icd9ComorbidShortRcppParallel(DataFrame icd9df, List icd9Mapping,
  std::string visitId = "visitId", std::string icd9Field = "icd9") {
  VB out;
  VecStr vs = as<VecStr>(as<CharacterVector>(icd9df[visitId]));
  VecStr icds = as<VecStr>(as<CharacterVector>(icd9df[icd9Field]));

  // TODO reserve size for map if possible for vcdb?
MapVecStr vcdb;
  // probably a slower initial step: create map of VecStr
  // (doesn't need to be sorted, but keys must be unique. Unsorted map though is C++11)
  std::string prevVisitId = "";
  int vlen = vs.size();
  VecStr vcodes; // codes for one visit
  for (int i=0; i<vlen; ++i) {
    if (vs[i]!=prevVisitId) {
      // no guarantee of order of visitId, so if the key already exists, we need to extend it
      MapVecStr::iterator mapit = vcdb.find(prevVisitId);
      if (mapit==vcdb.end()) {
        // first sight of this key, so just insert the accumulated codes:
        vcdb.insert(std::make_pair(prevVisitId, vcodes));
      } else {
        // already have the visitId in map, so update it (in place?!)
        VecStr old_codes = mapit->second;
        old_codes.insert(old_codes.begin(), vcodes.begin(), vcodes.end());
      }
      // now work on the next code
      vcodes.clear();
    }
    vcodes.push_back(icds[i]); // whether we wrote out the last visit codes, or have a new code
  }
  #ifdef ICD9_DEBUG
  std::cout << "visit map: vec icd9 created\n";
  #endif

  CharacterVector mapnames = icd9Mapping.names(); // could do this in worker instantiation.


  // convert mapping from List of CharacterVectors to std vector of sets. This
  // is a small one-off cost, and dramatically improves the performance of the
  // later loops, because we can .find() instead of linear search.
  CmbMap map;
  for (List::iterator mi = icd9Mapping.begin(); mi != icd9Mapping.end(); ++mi) {
    #ifdef ICD9_DEBUG
    std::cout << "working on building map..." << "\n";
    #endif
    VecStr mvs(as<VecStr>(*mi));
    SetStr ss(mvs.begin(), mvs.end());
    map.push_back(ss);
  }
  #ifdef ICD9_DEBUG
  std::cout << "reference mapping std structure created\n";
  #endif

  ComorbidWorker worker(vcdb, map, mapnames);
  #ifdef ICD9_DEBUG
  std::cout << "worker created\n";
  #endif
  // PARALLEL FOR HAS TO SPLIT UP WITH EACH VISITID TOGETHER: can't just slice whole list
  parallelFor(0, vcdb.size(), worker);
  #ifdef ICD9_DEBUG
  std::cout << "work complete\n";
  #endif

  //IntegerVector row_names = seq_len(as<LogicalVector>(out[1]).size());
  //out.attr("row.names") = row_names;
  //out.attr("class") = "data.frame";
  return out;
}

using namespace RcppParallel;

/**
 * * * * * initialize with the visitids:icd9s, comorbidity mapping
 * * * * * per thread:
 * * * * *    get thread's subset of the visits
 * * * * *    find unique visitIds (save in instance)
 * * * * *    create 'matrix' of logical comorbidities by searching the map for each code.
 * * * * *    update the master "matrix" by simply inserting the new values at the end
 * * * * * convert big bool vector into data.frame
 * * * * */
