// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::depends(RcppProgress)]]
// [[Rcpp::depends(RcppParallel)]]
#include <progress.hpp>
#include <Rcpp.h>
#include <string>
#include <RcppParallel.h>

using namespace Rcpp;
using namespace RcppParallel;

#define ICD9_DEBUG = 1

typedef std::vector<std::string > VecStr;
typedef VecStr::iterator VecStrIt;
typedef std::set<std::string > SetStr;
typedef std::map<int,std::string > MapStr;
typedef std::vector<SetStr > CmbMap;
typedef std::multimap<std::string, std::string> Tmm;
// internal function definitions
int printVecStr(VecStr sv);
int printCharVec(CharacterVector cv);

struct ComorbidWorker : public Worker {
  // source vectors
  Tmm vcdb;
  CmbMap map;
  CharacterVector mapnames;
  int nref;
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

    nref = map.size(); // the number of comorbidity groups

    // initialize with empty logical vectors
    LogicalVector cmb_all_false(usize, false); // inital vector of falses
    for (int cmb = 0; cmb < nref; ++cmb) {
      String cmbnm = mapnames[cmb];
      out[cmbnm] = cmb_all_false; // does data copy
    }

    for (int cmb = 0; cmb < nref; ++cmb) {
      LogicalVector cmbcol(usize, false); // inital vector of falses
      String cmbnm = mapnames[cmb];
      out[cmbnm] = cmbcol; // does data copy
    }
    // use std::multimap to get subset of icd codes for each visitId key
    for( Tmm::iterator it = vcdb.begin(); it != vcdb.end(); it = vcdb.upper_bound(it->first)) {

      // find the icd9 codes for a given visitId
      std::string key = it->first;
      std::pair <Tmm::iterator, Tmm::iterator> matchrange = vcdb.equal_range(key);

      int urow = distance(uvis.begin(), std::find(uvis.begin(), uvis.end(), key));

      // instead of assuming order of keys and counting, we need to insert the key, so the loop can go parallel
      // loop through comorbidities
      for (int cmb = 0; cmb < nref; ++cmb) {
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
    //for (CharacterVector::iterator it = mapnames.begin(); it != mapnames.end(); ++it) {
    for (int cmb = 0; cmb < nref; ++cmb) {
      //String cmb = *it;

      // does conv to stl types copy data? I don't think it should
      std::vector<bool> cmbcol = as<std::vector<bool> >(out[cmb]);
      std::vector<bool> rhscol = as<std::vector<bool> >(rhs.out[cmb]);
      cmbcol.insert(cmbcol.end(), rhscol.begin(), rhscol.end());
      // should have updated 'out' by referencing
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
std::string icd9Field = "icd9",
int threads = 4) {
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
    VecStr mvs(as<VecStr>(*mi));
    SetStr ss(mvs.begin(), mvs.end());
    map.push_back(ss);
  }
  #ifdef ICD9_DEBUG
  std::cout << "reference mapping std structure created\n";
  #endif

  //get unique visitIds so we can name and size the output, and also populate the visitId col of output
  VecStr uvis; // can this be made const?
  uvis.reserve(vcdb.size()); // over-reserve massively as first approximation
  for( Tmm::iterator it = vcdb.begin(); it != vcdb.end(); it = vcdb.upper_bound(it->first)) {
    uvis.insert(uvis.end(), it->first); // according to valgrind, this is the very slow step when uvis was a std::set
  }
  int usize = uvis.size();
  #ifdef ICD9_DEBUG
  std::cout << "got" << usize << "unique visitIds.\n";
  #endif

  ComorbidWorker worker(vcdb, map, mapnames);
  parallelFor(0, vcdb.size(), worker);

  mapnames.push_front("visitId"); // try to do this in the parallel part
  out.names() = mapnames;
  IntegerVector row_names = seq_len(usize);
  out.attr("row.names") = row_names;
  out.attr("class") = "data.frame";
  return out;
}

