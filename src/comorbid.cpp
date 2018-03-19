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

#include <Rcpp.h>
#include <string>                               // for string
#include <vector>                               // for vector, operator-
#include <iterator>
#include <algorithm>
#include "icd_types.h"                          // for VecVecInt, ComorbidOut
#include "local.h"                              // for buildMap, buildVisitC...
#include "config.h"                              // for buildMap, buildVisitC...
#include "util.h" // for valgrind helper
#include "comorbidCommon.h"
#include "comorbidSetup.h"
extern "C" {
#include "cutil.h"                              // for getRListOrDfElement
}

//' @rdname icd_comorbid
//' @description \code{\link{Rcpp}} approach to comorbidity assignment with
//'   OpenMP and vector of integers strategy. It is very fast, and most time is
//'   now spent setting up the data to be passed in.
//' @param aggregate single logical value, if \code{TRUE}, then take (possible
//'   much) more time to aggregate out-of-sequence visit IDs in the input
//'   data.frame. If this is \code{FALSE}, then each contiguous group of visit
//'   IDs will result in a row of comorbidities in the output data. If you know
//'   whether your visit IDs are disordered, then use \code{TRUE}.
//' @keywords internal
// [[Rcpp::export]]
SEXP icd9ComorbidShortCpp(const SEXP& icd9df, const Rcpp::List& icd9Mapping,
                          const std::string visitId, const std::string icd9Field,
                          const int threads = 8, const int chunk_size = 256,
                          const int omp_chunk_size = 1, bool aggregate = true) {

  valgrindCallgrindStart(false);
  debug_parallel();

  VecStr out_row_names; // size is reserved in buildVisitCodesVec
#ifdef ICD_DEBUG_SETUP
  Rcpp::Rcout << "building visit:codes structure\n";
#endif

  VecVecInt vcdb; // size is reserved later

  // http://gallery.rcpp.org/articles/reversing-a-vector/ Looks protection is
  // needed even though using C++ and Rcpp. And from Hadley's adv-r "if you don’t
  // protect every R object you create, the garbage collector will think they are
  // unused and delete them."
  const SEXP vsexp = PROTECT(getRListOrDfElement(icd9df, visitId.c_str()));
#ifdef ICD_DEBUG_SETUP
  Rcpp::Rcout << "type of vsexp = " << TYPEOF(vsexp) << "\n";
#endif
  if (TYPEOF(vsexp) != STRSXP) {
    Rcpp::stop("expecting visit ID in input data frame to be character vector");
    UNPROTECT(1); // vsexp
  }
  UNPROTECT(1); // vsexp not used further

  // build structure of patient data
  buildVisitCodesVec(icd9df, visitId, icd9Field, vcdb, out_row_names, aggregate);

  // build structure of comorbidity map data
  VecVecInt map;
  buildMap(icd9Mapping, map);

  const VecVecIntSz num_comorbid = map.size();
  const VecVecIntSz num_visits = vcdb.size();

#ifdef ICD_DEBUG_SETUP
  Rcpp::Rcout << num_visits << " visits\n";
  Rcpp::Rcout << num_comorbid << " is num_comorbid\n";
#endif
  VecInt out(vcdb.size() * map.size(), false);
  lookupComorbidByChunkFor(vcdb, map, chunk_size, omp_chunk_size, out);

#ifdef ICD_DEBUG
{
  Rcpp::Rcout << "out length is " << out.size() << "\n";
  int outsum = std::accumulate(out.begin(), out.end(), 0);
  Rcpp::Rcout << "out sum is " << outsum << "\n";
  Rcpp::Rcout << "Ready to convert to R Matrix\n";
}
#endif
// try cast to logical first. (in which case I can use char for Out)
std::vector<bool> intermed;
intermed.assign(out.begin(), out.end());
#ifdef ICD_DEBUG
Rcpp::Rcout << "converted from ComorbidOut to vec bool, so Rcpp can handle cast to R logical vector\n";
#endif
// matrix is just a vector with dimensions (and col major...) Hope this isn't a data copy.
Rcpp::LogicalVector mat_out = Rcpp::wrap(intermed);
mat_out.attr("dim") = Rcpp::Dimension((int) num_comorbid, (int) num_visits); // set dimensions in reverse (row major for parallel step)
mat_out.attr("dimnames") = Rcpp::List::create(icd9Mapping.names(), out_row_names);
// apparently don't need to set class as matrix here
Rcpp::Function t("t"); // use R transpose - seems pretty fast
#ifdef ICD_DEBUG
Rcpp::Rcout << "Ready to transpose and return\n";
#endif
Rcpp::LogicalVector transposed_out = t(mat_out);
valgrindCallgrindStop();
return transposed_out;
}
