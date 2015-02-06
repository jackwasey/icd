// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <local.h>
#include <string>
#include <RcppParallel.h>

using namespace RcppParallel;
using namespace Rcpp;

struct ComorbidWorker : public Worker {
  const MapVecStr vcdb;
  const CmbMap map;
  const CharacterVector mapnames;
  const CmbMap::size_type num_comorbid;
  const MapVecStr::size_type num_visits;
  VB out; // vector of bools we can restructure to a data.frame later

  // constructors
  ComorbidWorker(MapVecStr vcdb, CmbMap map, CharacterVector mapnames)
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
      std::string key = vis_it->first;
      VecStr codes = vis_it->second; // these are the ICD-9 codes for the current visitid

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
      for (CmbMap::size_type cmb = 0; cmb < num_comorbid; ++cmb) {
        #ifdef ICD9_TRACE
        std::cout << "working on cmb: " << cmb <<"... ";
        #endif
        // loop through icd codes for this visitId
        for (VecStrIt code_it = codes.begin(); code_it != codes.end(); ++code_it) {
          #ifdef ICD9_TRACE
          std::cout << "working on code: " << *code_it << "\n";
          printSetStr(map[cmb]);
          #endif
          if (map[cmb].find(*code_it) != map[cmb].end()) {
            VB::size_type out_idx = cmb*(num_visits-1) + urow;
            #ifdef ICD9_TRACE
            std::cout << "found match";
            std::cout << out.size() << ", but idx = " << out_idx << "\n";
            #endif
            if (out_idx < out.size()) {
            out[out_idx] = true; // and update the current item. This is where we define the matrix indexing to be by visitid first, then cmb, which fits with a dataframe of a list of columns.
            } else {
              std::cout << "out of bounds!\n";
            }
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
List icd9ComorbidShortRcppParallel(DataFrame icd9df, List icd9Mapping,
std::string visitId = "visitId", std::string icd9Field = "icd9") {
  VecStr vs = as<VecStr>(as<CharacterVector>(icd9df[visitId]));
  VecStr icds = as<VecStr>(as<CharacterVector>(icd9df[icd9Field]));

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

  // convert mapping from List of CharacterVectors to std vector of sets. This
  // is a small one-off cost, and dramatically improves the performance of the
  // later loops, because we can .find() instead of linear search.
  CmbMap map;
  for (List::iterator mi = icd9Mapping.begin(); mi != icd9Mapping.end(); ++mi) {
    VecStr mvs(as<VecStr>(*mi));
    SetStr ss(mvs.begin(), mvs.end());
    map.push_back(ss);
  }
  #ifdef ICD9_DEBUG
  std::cout << "reference comorbidity mapping STL structure created\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n";
  #endif

  ComorbidWorker worker(vcdb, map, mapnames);
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
  df_out["visitId"] = wrap(visitIds);
  // loop through comorbidities to extract logical vectors
  #ifdef ICD9_DEBUG
  int n = (int)worker.out.size();
  std::cout << "worker.out length = " << n <<"\n";
  std::cout << "worker.num_comorbid = " << worker.num_comorbid <<"\n";
  #endif

  for (size_t i=0;i<worker.num_comorbid;++i) {
    String cmb_name = mapnames[i];
    VB::iterator start = worker.out.begin();
    VB::iterator end = worker.out.begin();
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
