// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include <local.h>
#include <string>
using namespace Rcpp;

//' @rdname icd9Comorbid
//' @export
// [[Rcpp::export]]
List icd9ComorbidShort(
  DataFrame icd9df,
  List icd9Mapping,
  std::string visitId = "visitId", // or CharacterVector?
  std::string icd9Field = "icd9"
  ) {
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

    //get unique visitIds so we can name and size the output, and also populate the visitId col of output
    VecStr uvis;
    uvis.reserve(vcdb.size()); // over-reserve massively as first approximation
    for( Tmm::iterator it = vcdb.begin(); it != vcdb.end(); it = vcdb.upper_bound(it->first)) {
      uvis.insert(uvis.end(), it->first); // according to valgrind, this is the very slow step when uvis was a std::set
    }
    int usize = uvis.size();
    #ifdef ICD9_DEBUG
    std::cout << "got the following unique visitIds: ";
    //printVecStr(uvis);
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
    const LogicalVector cmb_all_false(usize, false); // inital vector of falses
    for (int cmb = 0; cmb < nref; ++cmb) {
      String cmbnm = mapnames[cmb];
      out[cmbnm] = cmb_all_false; // does data copy
    }

    // use std::multimap to get subset of icd codes for each visitId key
    //TODO: upper_bound jumps index irregularly
    for( Tmm::iterator it = vcdb.begin(); it != vcdb.end(); it = vcdb.upper_bound(it->first)) {

      // find the icd9 codes for a given visitId
      std::pair <Tmm::iterator, Tmm::iterator> matchrange;
      std::string key = it->first;
      matchrange = vcdb.equal_range(key);

      // get distance to the unique key, not the position of the key in the multimap
      //int urow = distance(vcdb.begin(), it);
      int urow = distance(uvis.begin(), std::find(uvis.begin(), uvis.end(), key)); //TODO make uvis a std::set to speed this up.

      // loop through comorbidities
      for (int cmb = 0; cmb < nref; ++cmb) {
        // loop through icd codes for this visitId
        for (Tmm::iterator j = matchrange.first; j != matchrange.second; ++j) {
          if (map[cmb].find(j->second) != map[cmb].end()) {
            LogicalVector cmbcol = out[cmb+1]; // does this copy?
            cmbcol[urow] = true; // cmbcol updates 'out' by reference (I think)
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

#ifdef ICD9_DEBUG
  void printVecStr(VecStr sv) {
    for (VecStrIt i=sv.begin();i!=sv.end();++i) {
      std::cout << *i << "\n";
    }
    return;
  }

  void printCharVec(CharacterVector cv) {
    for (CharacterVector::iterator i = cv.begin(); i != cv.end(); ++i) {
      String s = *i;
      std::cout << s.get_cstring() << "\n";
    }
    return;
  }
#endif
