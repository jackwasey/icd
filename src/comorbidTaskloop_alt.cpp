// Copyright (C) 2014 - 2018  Jack O. Wasey
//
// This file is part of icd.
//
// icd is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// icd is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with icd. If not, see <http://www.gnu.org/licenses/>.

// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::plugins(openmp)]]
//#include "comorbid_alt.h"              // for lookupComorbid_alt_ByChunkForTaskloop
#include "comorbidSetup.h"
#include <Rcpp.h>
#include <algorithm>                   // for binary_search, copy
#include <vector>                      // for vector, vector<>::const_iterator
#include "Rcpp/iostream/Rstreambuf.h"  // for Rcout
#include "icd_types.h"                 // for ComorbidOut, VecVecInt, VecVec...
#include "local.h"                     // for ICD_OPENMP
#include "config.h"                     // for valgrind, CXX11 etc
#include "util.h"                     // for debug_parallel

//' alternate comorbidity search
//'
//' alternate version using much simplified with Openmp taskloop, only in OMP4.5
//'
//' @keywords internal
// [[Rcpp::export]]
void lookupComorbid_alt_ByChunkForTaskloop(const VecVecInt& vcdb,
                                      const VecVecInt& map,
                                      NewOut& out) {
  debug_parallel_env();
  const VecVecIntSz num_comorbid = map.size();
  // https://stackoverflow.com/questions/2665936/is-there-a-way-to-specify-the-dimensions-of-a-nested-stl-vector-c
  // NewOut out(vcdb.size(), NewOutPt(num_comorbid));
  VecVecIntSz vis_i;
  VecInt::const_iterator code_it;

  // loop through all patient visits
#ifdef ICD_OPENMP
  // may need shared(out) but as I think each element can be written to
  // independently by different threads, try without.. private(vis_i)
  // superfluous?
#pragma omp taskloop shared(Rcpp::Rcout, out) //grainsize (256)
#endif
  for (vis_i = 0; vis_i < vcdb.size(); ++vis_i) {
    debug_parallel();
#ifdef ICD_DEBUG
    Rcpp::Rcout << "New visit: vis_i = " << vis_i << "\n";
#endif
    const VecInt& codes = vcdb[vis_i]; // these are the ICD-9 codes for the current visitid
    const VecIntIt cbegin = codes.begin();
    const VecIntIt cend = codes.end();

    // loop through this patient's ICD codes (almost always fewer codes per
    // patient than number of codes in one element of a comorbidity map)
    for (code_it = cbegin; code_it != cend; ++code_it) {
      // now loop through the comorbidities
      for (NewOutPt::size_type cmb = 0; cmb != num_comorbid; ++cmb) {
#ifdef ICD_DEBUG_TRACE
        Rcpp::Rcout << "vis_i = " << vis_i << "\n";
        Rcpp::Rcout << "cmb = " << cmb << "\n";
        Rcpp::Rcout << "this pt code# = " << std::distance(cbegin, code_it) << "\n";
#endif

        const VecInt& mapCodes = map[cmb]; // may be zero length
        // the maps were already sorted in comorbidSetup, to enable binary search with O(log n)
        bool found_it = std::binary_search(mapCodes.begin(), mapCodes.end(), *code_it);
        if (found_it) {
#ifdef ICD_DEBUG_TRACE
          Rcpp::Rcout << "found - " << "vis_i = " << vis_i
                      << " - cmb = " << cmb
                      << " - this pt code# = " << std::distance(cbegin, code_it)
                      << "\n";
#endif
#ifdef ICD_DEBUG
          out.at(vis_i).at(cmb) = true; // or 1 for VecInt
          Rcpp::Rcout << "Reading newly written value: " <<
            out.at(vis_i).at(cmb) << "\n";
#else
          out[vis_i][cmb] = true; // or 1 for VecInt
#endif
          break;
        } // end if found_it
      } // end loop through comorbidities
    } // end loop through all ICD codes for one patient
#ifdef ICD_DEBUG
    printIt(out.at(vis_i));
#endif
  } // end main loop through patient visits
}

//' Simpler comorbidity assignment
//'
//' Re-written without OpenMP initially, but structured more simply, with the motivation of
//' using modern compiler features and OpenMP 4.5 with 'taskloop' construct.
//' \url{https://developers.redhat.com/blog/2016/03/22/what-is-new-in-openmp-4-5-3/}
//'
//' # basic test
//' # use tests/testthat/helper-base.R for two_pts and two_map
//' comorbid(two_pts, two_map, comorbid_fun = icd:::icd9Comorbid_alt_Taskloop)
//' @keywords internal
// [[Rcpp::export]]
SEXP icd9Comorbid_alt_Taskloop(const SEXP& icd9df, const Rcpp::List& icd9Mapping,
                          const std::string visitId, const std::string icd9Field,
                          const int threads = 8, const int chunk_size = 256,
                          const int omp_chunk_size = 1) {
  valgrindCallgrindStart(false);
  VecStr out_row_names; // size is reserved in buildVisitCodesVec
  VecVecInt vcdb; // size is reserved later

  const SEXP vsexp = PROTECT(getRListOrDfElement(icd9df, visitId.c_str()));
  if (TYPEOF(vsexp) != STRSXP) {
    Rcpp::stop("expecting visit ID in input data frame to be character vector");
    UNPROTECT(1); // vsexp
  }
  UNPROTECT(1); // vsexp not used further
#ifdef ICD_DEBUG
  Rcpp::Rcout << "build structure of patient data, into vcdb\n";
#endif

  buildVisitCodesVec(icd9df, visitId, icd9Field, vcdb, out_row_names);

#ifdef ICD_DEBUG
  Rcpp::Rcout << "build structure of comorbidity map data. This could be cached or memoised somehow\n";
#endif

  VecVecInt map;
  buildMap(icd9Mapping, map);

  const VecVecIntSz num_comorbid = map.size();
  const VecVecIntSz num_visits = vcdb.size();

#ifdef ICD_DEBUG
  Rcpp::Rcout << "num_comorbid = " << num_comorbid << "\n";
  Rcpp::Rcout << "num_visits = " << num_visits << "\n";
  Rcpp::Rcout << "look up the comorbidities\n";
#endif
  NewOut out(vcdb.size(), NewOutPt(num_comorbid));
  lookupComorbid_alt_ByChunkForTaskloop(vcdb, map, out);
  Rcpp::LogicalMatrix mat_out(num_visits, num_comorbid);
  for (NewOutIt it = out.begin(); it != out.end(); ++it) {

#ifdef ICD_DEBUG_TRACE
    printIt(*it);
    Rcpp::Rcout << "2nd val in vector: " << it->at(1);
    R_FlushConsole();
    R_ProcessEvents();
    R_CheckUserInterrupt();
    Rcpp::Rcout << " Injecting out data into matrix:\n";
    R_FlushConsole();
    R_ProcessEvents();
    R_CheckUserInterrupt();
#endif
    NewOutPt out_one_pt;
    for (VecVecIntSz cmb = 0; cmb != num_comorbid; ++cmb) {
      out_one_pt = *it;
#ifdef ICD_DEBUG_TRACE
      Rcpp::Rcout << "first vector has length " << out_one_pt.size() <<
        " - ptid = " << std::distance(out.begin(), it) <<
          " - comorbidity # " << cmb <<
            " val = " << out_one_pt.at(cmb) << "\n";
#endif
#ifdef ICD_DEBUG
      mat_out(std::distance(out.begin(), it), cmb) = out_one_pt.at(cmb);
#else
      mat_out(std::distance(out.begin(), it), cmb) = out_one_pt[cmb];
#endif
    }
  }
  mat_out.attr("dim") = Rcpp::Dimension((int) num_visits, (int) num_comorbid); // set dimensions in usual column major order
  mat_out.attr("dimnames") = Rcpp::List::create(out_row_names, icd9Mapping.names());
  valgrindCallgrindStop();
  return mat_out;
}

//' @describeIn icd9Comorbid_alt_Taskloop Taskloop but finish with R transpose
//' @keywords internal
// [[Rcpp::export]]
SEXP icd9Comorbid_alt_Taskloop2(const SEXP& icd9df, const Rcpp::List& icd9Mapping,
                           const std::string visitId, const std::string icd9Field,
                           const int threads = 8, const int chunk_size = 256,
                           const int omp_chunk_size = 1) {

  valgrindCallgrindStart(false);
  VecStr out_row_names; // size is reserved in buildVisitCodesVec
  VecVecInt vcdb; // size is reserved later

  const SEXP vsexp = PROTECT(getRListOrDfElement(icd9df, visitId.c_str()));
  if (TYPEOF(vsexp) != STRSXP) {
    Rcpp::stop("expecting visit ID in input data frame to be character vector");
    UNPROTECT(1); // vsexp
  }
  UNPROTECT(1); // vsexp not used further
  buildVisitCodesVec(icd9df, visitId, icd9Field, vcdb, out_row_names);

  VecVecInt map;
  buildMap(icd9Mapping, map);

  const VecVecIntSz num_comorbid = map.size();
  const VecVecIntSz num_visits = vcdb.size();
  NewOut out(vcdb.size(), NewOutPt(num_comorbid));
  lookupComorbid_alt_ByChunkForTaskloop(vcdb, map, out);

  Rcpp::IntegerMatrix mat_out(num_visits, num_comorbid);
  my_concat(out.begin(), out.end(), mat_out.begin()); // write out row major
  mat_out.attr("dim") = Rcpp::Dimension((int) num_comorbid, (int) num_visits); // set dimensions in usual column major order
  mat_out.attr("dimnames") = Rcpp::List::create(icd9Mapping.names(), out_row_names);
  Rcpp::IntegerMatrix transposed_out = Rcpp::transpose(mat_out); // doesn't look efficient...
  valgrindCallgrindStop();
  return transposed_out;
}
