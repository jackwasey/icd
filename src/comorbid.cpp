#include <Rcpp.h>
#include <string>
#include <boost/multi_index_container.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/multi_index/identity.hpp>
#include <boost/multi_index/member.hpp>

using namespace Rcpp;
using namespace boost::multi_index;

#define DEBUG = true

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

typedef std::vector<std::string > VecStr;
typedef VecStr::iterator VecStrIt;
typedef std::set<std::string > SetStr;
typedef std::vector<SetStr > VecSetStr;
typedef std::multimap<std::string, std::string> Tmm;
typedef std::vector<std::vector<bool > > Vvb;


int printVecStr(VecStr sv);
int printCharVec(CharacterVector cv);
//List addListElementByRef(List& df, std::string name);

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

    //loop through visit and icd codes together
    VecStrIt j = icds.begin();
    for (VecStrIt i = vs.begin(); i != vs.end(); ++i, ++j) {
      vcdb.insert(std::pair<std::string, std::string>(*i, *j));
    }

    #ifdef DEBUG
    std::cout << "multimap created\n";
    #endif
    //get unique visitIds so we can name and size the output
    VecStr uvis(vs.size());
    int pos;
    for( Tmm::iterator it = vcdb.begin(); it != vcdb.end(); it = vcdb.upper_bound(it->first)) {
      pos = distance(vcdb.begin(), it);
      uvis[pos] = it->first;
    }
    int usize = pos+1;
    uvis.resize(usize);

    #ifdef DEBUG
    std::cout << "got the following unique visitIds: ";
    printVecStr(uvis);
    #endif

    // convert mapping from List of CharacterVectors to std vector of sets. This
    // is a small one-off cost, and dramatically improves the performance of the
    // later loops, because we can .find() instead of linear search.
    VecSetStr map;
    for (List::iterator mi = icd9Mapping.begin(); mi != icd9Mapping.end(); ++mi) {
      VecStr mvs(as<VecStr>(*mi));
      SetStr ss(mvs.begin(), mvs.end());
      map.push_back(ss);
    }
    #ifdef DEBUG
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
      //addListElementByRef(out, cmbnm);
    }

    // use std::multimap to get subset of icd codes for each visitId key
    for( Tmm::iterator it = vcdb.begin(); it != vcdb.end(); it = vcdb.upper_bound(it->first)) {

      // find the icd9 codes for a given visitId
      std::pair <Tmm::iterator, Tmm::iterator> matchrange;
      matchrange = vcdb.equal_range(it->first);

      // loop through comorbidities
      for (int cmb = 0; cmb < nref; ++cmb) {
        // loop through icd codes for this visitId
        for (Tmm::iterator j = matchrange.first; j != matchrange.second; ++j) {
          if (map[cmb].find(j->second) != map[cmb].end()) {
            int urow = distance(vcdb.begin(), it) ;
            LogicalVector cmbcol = out[cmb+1]; // does this copy?
            cmbcol[urow] = true;
          }
        }
      }
    }
    mapnames.push_front("visitId");
    //    #ifdef DEBUG
    //    std::cout << "mapping names\n";
    //    printCharVec(mapnames);
    //    #endif
    out.names() = mapnames;
    IntegerVector row_names = seq_len(usize);
    out.attr("row.names") = row_names;
    out.attr("class") = "data.frame";
    return out;
  }

  int printVecStr(VecStr sv) {
    for (VecStr::iterator i = sv.begin(); i != sv.end(); ++i) {
      std::cout << *i << "\n";
    }
    return 0;
  }

  int printCharVec(CharacterVector cv) {
    for (CharacterVector::iterator i = cv.begin(); i != cv.end(); ++i) {
      String s = *i;
      std::cout << s.get_cstring() << "\n";
    }
    return 0;
  }

// this is from: http://stackoverflow.com/questions/15731106/passing-by-reference-a-data-frame-and-updating-it-with-rcpp
// updates a data frame, note copying is with pointers, not the whole df
// [[Rcpp::export]]
//List addListElementByRef(List& df, std::string name) {
//    CharacterVector v = df[0];
//    int nr = v.size();
//    int nc = df.size() ;
//    LogicalVector newCol(nr, false);
//    List out(nc+1) ;
//    CharacterVector onames = df.attr("names") ;
//    CharacterVector names( nc + 1 ) ;
//    for( int i=0; i<nc; i++) {
//        out[i] = df[i] ; // copying by reference
//        names[i] = onames[i] ;
//    }
//    out[nc] = newCol ;
//    names[nc] = name ;
//    out.attr("names") = names ;
//    return out ;
//}
