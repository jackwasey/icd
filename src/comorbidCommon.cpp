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
#include "comorbidCommon.h"
#include <Rcpp.h>
#include <algorithm>                   // for binary_search, copy
#include <vector>                      // for vector, vector<>::const_iterator
#include "Rcpp/iostream/Rstreambuf.h"  // for Rcout
#include "icd_types.h"                 // for ComorbidOut, VecVecInt, VecVec...
#include "local.h"                     // for ICD_OPENMP
#include "config.h"                     // for valgrind, CXX11 etc
#include "util.h"                     // for debug_parallel

//' core search for ICD code in a map
//' @keywords internal
// [[Rcpp::export]]
void lookupComorbidByChunkFor(const VecVecInt& vcdb,
                              const VecVecInt& map,
                              const VecVecIntSz chunkSize,
                              const VecVecIntSz ompChunkSize,
                              VecInt& out) {
  const VecVecIntSz num_comorbid = map.size();
  const VecVecIntSz last_i = vcdb.size() - 1;
  VecVecIntSz chunk_end_i;
  VecVecIntSz vis_i;
  const VecVecIntSz vsz = vcdb.size();

#ifdef ICD_DEBUG_TRACE
  Rcpp::Rcout << "vcdb.size() = " << vcdb.size() << "\n";
  Rcpp::Rcout << "map.size() = " << map.size() << "\n";
#endif
  debug_parallel();

#ifdef ICD_OPENMP
#pragma omp parallel for schedule(static) default(none) shared(out, Rcpp::Rcout, vcdb, map) private(chunk_end_i, vis_i)
  // SOMEDAY: need to consider other processes using multiple cores, see Writing R Extensions.
  //	omp_set_schedule(omp_sched_static, ompChunkSize);
#endif
  // loop through chunks at a time, by integer size:
  // https://stackoverflow.com/questions/2513988/iteration-through-std-containers-in-openmp
  for (vis_i = 0; vis_i < vsz; vis_i += chunkSize) {
#ifdef ICD_DEBUG_TRACE
    Rcpp::Rcout << "vis_i = " << vis_i << "\n";
#endif
    debug_parallel();
    // chunk end is an index, so for zero-based vis_i and chunk_end should be
    // the last index in the chunk
    chunk_end_i = vis_i + chunkSize - 1;
    if (chunk_end_i > last_i)
      chunk_end_i = last_i;
    VecInt chunk;
#ifdef ICD_DEBUG_TRACE
    Rcpp::Rcout << "OMP vcdb.size() = " << vcdb.size() << "\n";
    Rcpp::Rcout << "OMP map.size() = " << map.size() << "\n";
#endif
    const VecVecIntSz& begin = vis_i;
    const VecVecIntSz& end = chunk_end_i;

#ifdef ICD_DEBUG_TRACE
    Rcpp::Rcout << "lookupComorbidChunk begin = " << begin << ", end = " << end << "\n";
#endif
    const VecInt falseComorbidChunk(num_comorbid * (1 + end - begin), false);
    chunk = falseComorbidChunk;
    for (VecVecIntSz urow = begin; urow <= end; ++urow) { //end is index of end of chunk, so we include it in the loop.
      for (VecVecIntSz cmb = 0; cmb != num_comorbid; ++cmb) { // loop through icd codes for this visitId
#ifdef ICD_DEBUG_TRACE
        Rcpp::Rcout << "row: " << 1 + urow - begin << " of " << 1 + end - begin << ", ";
        Rcpp::Rcout << "cmb = " << cmb << "\n";
        Rcpp::Rcout << "vcdb length in lookupOneChunk = " << vcdb.size() << "\n";
        Rcpp::Rcout << "map length in lookupOneChunk = " << map.size() << "\n";
#endif

        const VecInt& codes = vcdb[urow]; // these are the ICD-9 codes for the current visitid
        const VecInt& mapCodes = map[cmb]; // may be zero length

        const VecInt::const_iterator cbegin = codes.begin();
        const VecInt::const_iterator cend = codes.end();
        for (VecInt::const_iterator code_it = cbegin; code_it != cend; ++code_it) {
          // the maps were already sorted, now binary search is actually only O(log n)
          bool found_it = std::binary_search(mapCodes.begin(), mapCodes.end(), *code_it);
          if (found_it) {
            const VecInt::size_type chunk_idx = num_comorbid
            * (urow - begin) + cmb;
            // chunk.at(chunk_idx) = true; // for debug if going OOB
            chunk[chunk_idx] = true;
            break;
          } // end if found_it
        } // end loop through codes in one comorbidity
      } // end loop through all comorbidities
    } // end loop through visits
#ifdef ICD_DEBUG_TRACE
    Rcpp::Rcout << "finished with one chunk\n";
#endif

    // next block doesn't need to be single threaded(?), but doing so improves
    // cache contention
#ifdef ICD_OPENMP
#pragma omp critical
#endif
{
#ifdef ICD_DEBUG_TRACE
  Rcpp::Rcout << "writing a chunk beginning at: " << vis_i << "\n";
#endif
  // write calculated data to the output matrix (must sync threads before this)
  std::copy(chunk.begin(), chunk.end(), out.begin() + (num_comorbid * vis_i));
}
  } // end parallel for

#ifdef ICD_DEBUG
  Rcpp::Rcout << "finished looking up all chunks in for loop\n";
#endif
}


//' alternate comorbidity search
//'
//' alternate version using much simplified with Openmp taskloop, only in OMP4.5
//'
//' @keywords internal
// [[Rcpp::export]]
void lookupComorbidByChunkForTaskloop(const VecVecInt& vcdb,
                                      const VecVecInt& map,
                                      NewOut& out) {
  const VecVecIntSz num_comorbid = map.size();
  VecVecIntSz vis_i;
  VecInt::const_iterator code_it;

 // ensure out is big enough:
 out.resize(vcdb.size());

  // loop through all patient visits
#ifdef ICD_OPENMP
#pragma omp taskloop grainsize (256)
#endif
  for (vis_i = 0; vis_i != vcdb.size(); ++vis_i) {
#ifdef ICD_DEBUG
    Rcpp::Rcout << "New visit: vis_i = " << vis_i << "\n";
#endif
    const VecInt& codes = vcdb[vis_i]; // these are the ICD-9 codes for the current visitid
    const VecIntIt cbegin = codes.begin();
    const VecIntIt cend = codes.end();

    // usually avoid vector<bool>, but could avoid later conversion (maybe?)
    // required by Rcpp, no thread competition in this inner loop, and it is
    // compact. Initialize with false/0.
    NewOutPt out_one_pt;
    out_one_pt.assign(num_comorbid, false); // fill with false/0

    // loop through this patient's ICD codes (almost always fewer codes per
    // patient than number of codes in one element of a comorbidity map)
    for (code_it = cbegin; code_it != cend; ++code_it) {
      // now loop through the comorbidities
      for (VecVecIntSz cmb = 0; cmb != num_comorbid; ++cmb) {
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
          Rcpp::Rcout << "found - ";
#endif
#ifdef ICD_DEBUG_TRACE
          Rcpp::Rcout << "vis_i = " << vis_i << " - ";
          Rcpp::Rcout << "cmb = " << cmb << " - ";
          Rcpp::Rcout << "this pt code# = " << std::distance(cbegin, code_it) << "\n";
#endif
          out_one_pt.at(cmb) = true; // or 1 for VecInt
          break;
        } // end if found_it
      } // end loop through comorbidities
    } // end loop through all ICD codes for one patient

#ifdef ICD_DEBUG
    Rcpp::Rcout << "Writing: out.size = " << out.size() << ", and vis_i = " << vis_i << "\n";
    printIt(out_one_pt);
#endif
    out.at(vis_i) = out_one_pt;
  } // end main loop through patient visits
}
