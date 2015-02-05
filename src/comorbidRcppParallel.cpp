// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <local.h>
#include <string>
#include <RcppParallel.h>

using namespace Rcpp;
using namespace RcppParallel;

/**
 * * * initialize with the visitids:icd9s, comorbidity mapping
 * * * per thread:
 * * *    get thread's subset of the visits
 * * *    find unique visitIds (save in instance)
 * * *    create 'matrix' of logical comorbidities by searching the map for each code.
 * * *    update the master "matrix" by simply inserting the new values at the end
 * * * convert big bool vector into data.frame
 * * */

 struct ComorbidWorker : public Worker {
   Tmm vcdb;
   CmbMap map;
   CharacterVector mapnames;
   int num_comorbid;
   VB out; // vector of bools we can restructure to a data.frame later
   VecStr unique_visits;
   //static const VecStr findUniqueVisits(Tmm vcdb, Tmm::iterator chunkbegin, Tmm::iterator chunkend);

   // constructors
   ComorbidWorker(Tmm vcdb, CmbMap map, CharacterVector mapnames)
   : vcdb(vcdb), map(map), mapnames(mapnames) {}

   static const VecStr findUniqueVisits(Tmm vcdb, Tmm::iterator chunkbegin, Tmm::iterator chunkend) {
     //get unique visitIds so we can name and size the output, and also populate the visitId col of output
     VecStr uvis;
     uvis.reserve(std::distance(chunkbegin, chunkend)); // over-reserve massively as first approximation (i.e. 1 comorbid per pt)
     for(Tmm::iterator it = chunkbegin; it != chunkend; it = vcdb.upper_bound(it->first)) {
       uvis.insert(uvis.end(), it->first); // according to valgrind, this is the very slow step when unique_visits was a std::set
     }
     return uvis;
   }

   void operator()(std::size_t begin, std::size_t end) {
     // do the work for the given range

     Tmm::iterator chunkbegin = vcdb.begin();
     Tmm::iterator chunkend = vcdb.begin();
     advance(chunkbegin, begin);
     advance(chunkend, end-1);

     const int num_u_vis = unique_visits.size();
     #ifdef ICD9_DEBUG
     std::cout << "got the following unique visitIds: ";
     std::cout << num_u_vis << "\n";
     #endif

     num_comorbid = map.size(); // the number of comorbidity groups

     // initialize the output 'matrix' vector (num unique visits * num comorbidities)
     // superfluous? out.reserve(num_u_vis*num_comorbid);
     out = std::vector<bool>(num_u_vis*num_comorbid, false); // initialize to FALSE

     // use std::multimap to get subset of icd codes for each visitId key
     for(Tmm::iterator it = chunkbegin; it != chunkend; it = vcdb.upper_bound(it->first)) {

       // find the icd9 codes for a given visitId
       std::string key = it->first;
       std::pair <Tmm::iterator, Tmm::iterator> matchrange = vcdb.equal_range(key);

       #ifdef ICD9_DEBUG
       std::cout << "working on key: " << key << "\n";
       #endif

// the Tmm iterator counts down whole list of (probably duplicated) visits, so we need to keep track:
       int urow = distance(unique_visits.begin(), std::find(unique_visits.begin(), unique_visits.end(), key));

       // instead of assuming order of keys and counting, we need to insert the key, so the loop can go parallel
       // loop through comorbidities
       for (int cmb = 0; cmb < num_comorbid; ++cmb) {
         // loop through icd codes for this visitId
         for (Tmm::iterator j = matchrange.first; j != matchrange.second; ++j) {
           if (map[cmb].find(j->second) != map[cmb].end()) {
             out[cmb*num_comorbid + urow] = true; // and update the current 'row'
           }
         }
       }
     }
   }

   void join(ComorbidWorker& rhs) {
     // now insert vectors from each col of RHS into out
     for (int cmb = 1; cmb < num_comorbid; ++cmb) { // visitId is col 0, TODO: go to n+1
     #ifdef ICD9_DEBUG
     cout << "working on joining cmb: " << cmb << "\n";
     #endif
     // does conv to stl types copy data? I don't think it should
     out.insert(out.end(), rhs.out.begin(), rhs.out.end());
     //for (CharacterVector::iterator it = lvc.begin(); it != lvs.end(); ++it)
     //std::vector<bool> cmbcol = as<std::vector<bool> >(out[cmb]);
     //std::vector<bool> rhscol = as<std::vector<bool> >(rhs.out[cmb]);
     //cmbcol.insert(cmbcol.end(), rhscol.begin(), rhscol.end()); // don't insert just for testing
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

