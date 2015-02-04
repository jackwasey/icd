// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <local.h>
#include <string>
#include <RcppParallel.h>

using namespace Rcpp;
using namespace RcppParallel;

struct ComorbidWorker : public Worker {
  // source vectors
  Tmm vcdb;
  CmbMap map;
  CharacterVector mapnames;
  int num_comorbid;
  List out; // it's actually a proto-data.frame

  // constructors
  ComorbidWorker(Tmm vcdb, CmbMap map, CharacterVector mapnames)
  : vcdb(vcdb), map(map), mapnames(mapnames), out(List::create()) {}

  void operator()(std::size_t begin, std::size_t end) {
    // do the work for the given range

    //get unique visitIds so we can name and size the output, and also populate the visitId col of output
    VecStr uvis; // can this be made const?
    uvis.reserve(vcdb.size()); // over-reserve massively as first approximation
    for( Tmm::iterator it = vcdb.begin(); it != vcdb.end(); it = vcdb.upper_bound(it->first)) {
      uvis.insert(uvis.end(), it->first); // according to valgrind, this is the very slow step when uvis was a std::set
    }
    int usize = uvis.size();
    #ifdef ICD9_DEBUG
    std::cout << "got the following unique visitIds: ";
    std::cout << usize << "\n";
    #endif

    num_comorbid = map.size(); // the number of comorbidity groups

    // initialize the output List
    out["visitId"] = uvis;
    // initialize the rest with empty logical vectors
    const LogicalVector empty_comorbid(usize, false); // inital vector of falses
    for (int cmb = 0; cmb < num_comorbid; ++cmb) {
      const String cmbnm = mapnames[cmb];
      out[cmbnm] = empty_comorbid; // copies empty vectors into the result list/data.frame
    }

    // use std::multimap to get subset of icd codes for each visitId key
    for( Tmm::iterator it = vcdb.begin(); it != vcdb.end(); it = vcdb.upper_bound(it->first)) {

      // find the icd9 codes for a given visitId
      std::string key = it->first;
      std::pair <Tmm::iterator, Tmm::iterator> matchrange = vcdb.equal_range(key);

      int urow = distance(uvis.begin(), std::find(uvis.begin(), uvis.end(), key));

      // instead of assuming order of keys and counting, we need to insert the key, so the loop can go parallel
      // loop through comorbidities
      for (int cmb = 0; cmb < num_comorbid; ++cmb) {
        // loop through icd codes for this visitId
        for (Tmm::iterator j = matchrange.first; j != matchrange.second; ++j) {
          if (map[cmb].find(j->second) != map[cmb].end()) {
            LogicalVector cmbcol = out[cmb+1]; // get referemce tp current LogicalVector of comorbities
            cmbcol[urow] = true; // and update the current row
          }
        }
      }
    }



  }

  void join(ComorbidWorker& rhs) {
    // now insert vectors from each col of RHS into out
    for (int cmb = 0; cmb < num_comorbid; ++cmb) {
      // does conv to stl types copy data? I don't think it should
      std::vector<bool> cmbcol = as<std::vector<bool> >(out[cmb]);
      std::vector<bool> rhscol = as<std::vector<bool> >(rhs.out[cmb]);
      cmbcol.insert(cmbcol.end(), rhscol.begin(), rhscol.end());
      // should have updated 'out' by reference
    }
  }
};

//' @rdname icd9Comorbid
//' @description ParallelTwo will try to use built in STL parallelism
//' @export
// [[Rcpp::export]]
List icd9ComorbidShortRcppParallel(DataFrame icd9df,
List icd9Mapping,
std::string visitId = "visitId", // or CharacterVector?
std::string icd9Field = "icd9") {
  List out;
  VecStr vs = as<VecStr>(as<CharacterVector>(icd9df[visitId]));
  VecStr icds = as<VecStr>(as<CharacterVector>(icd9df[icd9Field]));

  CharacterVector mapnames = icd9Mapping.names();


  // create a multimap of visitid-code pairs
  Tmm vcdb;
  //loop through visit and icd codes and put together
  VecStrIt j = icds.begin();
  for (VecStrIt i = vs.begin(); i != vs.end(); ++i, ++j) {
    vcdb.insert(std::pair<std::string, std::string>(*i, *j));
  }
  #ifdef ICD9_DEBUG
  std::cout << "multimap created\n";
  #endif

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
  parallelFor(0, vcdb.size(), worker);
  #ifdef ICD9_DEBUG
  std::cout << "work complete\n";
  #endif

  //IntegerVector row_names = seq_len(as<LogicalVector>(out[1]).size());
  //out.attr("row.names") = row_names;
  //out.attr("class") = "data.frame";
  return out;
}

