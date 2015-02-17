// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::plugins(openmp)]]
#include <Rcpp.h>
#include <local.h>
#include <string>

#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

//' @rdname icd9Comorbid
//' @description ParallelOpenMP invokes openmp at visitId level loop for only 25% speed-up with 4 threads.
//' I'm pretty sure this is bad because STL is not thread safe.
//' @export
// [[Rcpp::export]]
List icd9ComorbidShortOpenMP(
  DataFrame icd9df,
  List icd9Mapping,
  std::string visitId = "visitId", // or CharacterVector?
  std::string icd9Field = "icd9",
  int threads = 4
  ) {
    List out;
    VecStr vs = as<VecStr>(as<CharacterVector>(icd9df[visitId]));
    VecStr icds = as<VecStr>(as<CharacterVector>(icd9df[icd9Field]));
    CharacterVector mapnames = icd9Mapping.names();

    #ifdef _OPENMP
    #ifdef ICD9_DEBUG
    REprintf("_OPENMP is defined. Setting threads.\n");
    #endif
    if (threads > 0)
    omp_set_num_threads(threads);
    #ifdef ICD9_DEBUG
    REprintf("Number of threads=%i\n", omp_get_max_threads());
    #endif
    #endif

    // create a multimap of visitid-code pairs
    MapVisitCode vcdb;
    //loop through visit and icd codes and put together
    VecStr::iterator j = icds.begin();
    for (VecStr::iterator i = vs.begin(); i != vs.end(); ++i, ++j) {
      vcdb.insert(std::pair<std::string, std::string>(*i, *j));
    }
    #ifdef ICD9_DEBUG
    std::cout << "multimap created\n";
    #endif

    //get unique visitIds so we can name and size the output, and also populate the visitId col of output
    VecStr uvis;
    uvis.reserve(vcdb.size()); // over-reserve massively as first approximation
    for( MapVisitCode::iterator it = vcdb.begin(); it != vcdb.end(); it = vcdb.upper_bound(it->first)) {
      uvis.insert(uvis.end(), it->first); // according to valgrind, this is the very slow step when uvis was a std::set
    }
    int usize = uvis.size();
    #ifdef ICD9_DEBUG
    std::cout << "got the following number of unique visitIds: ";
    std::cout << usize << "\n";
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

    int nref = map.size(); // the number of comorbidity groups
    out = List::create(); // start building data.frame output as list.
    out[visitId] = uvis;

    // initialize with empty logical vectors
    for (int cmb = 0; cmb < nref; ++cmb) {
      LogicalVector cmbcol(usize, false); // inital vector of falses
      String cmbnm = mapnames[cmb];
      out[cmbnm] = cmbcol; // does data copy
    }

    // use std::multimap to get subset of icd codes for each visitId key
    //TODO: upper_bound jumps index irregularly
    for( MapVisitCode::iterator it = vcdb.begin(); it != vcdb.end(); it = vcdb.upper_bound(it->first)) {
      // find the icd9 codes for a given visitId
      std::pair <MapVisitCode::iterator, MapVisitCode::iterator> matchrange;
      std::string key = it->first;
      matchrange = vcdb.equal_range(key);

      // get distance to the unique key, not the position of the key in the multimap
      //int urow = distance(vcdb.begin(), it);
      int urow = distance(uvis.begin(), std::find(uvis.begin(), uvis.end(), key)); //TODO make uvis a std::set to speed this up.

      // loop through comorbidities
      #pragma omp parallel for schedule(static) // dynamic may be better suited than static.
      for (int cmb = 0; cmb < nref; ++cmb) {
        // loop through icd codes for this visitId
        for (MapVisitCode::iterator j = matchrange.first; j != matchrange.second; ++j) {
          if (map[cmb].find(j->second) != map[cmb].end()) {
            LogicalVector cmbcol = out[cmb+1]; // NOT THREAD SAFE?!
            cmbcol[urow] = true;
          }
        }
      }
    }
    mapnames.push_front("visitId");
    out.names() = mapnames;
    IntegerVector row_names = seq_len(usize);
    out.attr("row.names") = row_names;
    out.attr("class") = "data.frame";
    return out;
  }

