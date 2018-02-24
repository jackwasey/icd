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

#include <Rcpp/r/headers.h>                     // for TYPEOF, Rf_install, SEXP
#include <string>                               // for string
#include <vector>                               // for vector, operator-
#include "Rcpp.h"                               // for wrap
#include "Rcpp/Dimension.h"                     // for Dimension
#include "Rcpp/Function.h"                      // for Function, Function_Impl
#include "Rcpp/api/meat/Dimension.h"            // for Dimension::operator S...
#include "Rcpp/api/meat/proxy.h"                // for AttributeProxyPolicy:...
#include "Rcpp/exceptions.h"                    // for stop
#include "Rcpp/generated/Function__operator.h"  // for Function_Impl::operat...
#include "Rcpp/generated/Vector__create.h"      // for Vector::create
#include "Rcpp/generated/grow__pairlist.h"      // for pairlist
#include "Rcpp/proxy/AttributeProxy.h"          // for AttributeProxyPolicy<...
#include "Rcpp/proxy/NamesProxy.h"              // for NamesProxyPolicy<>::c...
#include "Rcpp/vector/Vector.h"                 // for Vector
#include "Rcpp/vector/instantiation.h"          // for LogicalVector, List
#include "icd_types.h"                          // for VecVecInt, ComorbidOut
#include "local.h"                              // for buildMap, buildVisitC...
extern "C" {
  #include "cutil.h"                              // for getRListOrDfElement
}

#ifdef ICD_DEBUG_PARALLEL
#include "util.h"
#endif
//#include <Rcpp.h>

// R CMD INSTALL --no-docs icd && R -e "library(icd); icd:::runOpenMPVecInt();"

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
#ifdef ICD_VALGRIND
#ifdef ICD_DEBUG
  Rcpp::Rcout << "Starting valgrind instrumentation... ";
#endif
  CALLGRIND_START_INSTRUMENTATION;
  if (FALSE) {
    Rcpp::Rcout << "Zeroing stats... ";
    CALLGRIND_ZERO_STATS;
  }
#endif
#if (defined ICD_DEBUG_SETUP || defined ICD9_SETUP)
  Rcpp::Rcout << "icd9ComorbidShortOpenMPVecInt\n";
  Rcpp::Rcout << "chunk size = " << chunk_size << "\n";
#endif

#ifdef ICD_DEBUG_PARALLEL
  Rcpp::Rcout << "checking _OPENMP... ";
#ifdef _OPENMP
  Rcpp::Rcout << "_OPENMP is defined.\n";
#else
  Rcpp::Rcout << "_OPENMP is not defined.\n";
#endif
#ifdef ICD_OPENMP
  Rcpp::Rcout << "ICD_OPENMP is defined.\n";
#else
  Rcpp::Rcout << "ICD_OPENMP is not defined.\n";
#endif
  debug_parallel();
#endif

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
  if (TYPEOF(vsexp) != STRSXP)
    Rcpp::stop("expecting visit ID in input data frame to be character vector");
  UNPROTECT(1); // vsexp not used further

#ifdef ICD_DEBUG_SETUP
  Rcpp::Rcout << "icd9ComorbidShortMatrix STRSXP\n";
#endif
  buildVisitCodesVec(icd9df, visitId, icd9Field, vcdb, out_row_names,
                     aggregate);

#ifdef ICD_DEBUG_SETUP
  Rcpp::Rcout << "building icd9Mapping\n";
#endif
  VecVecInt map;
  buildMap(icd9Mapping, map);

#ifdef ICD_DEBUG_SETUP
  Rcpp::Rcout << "first cmb has len: " << map[0].size() << "\n";
#endif

  const VecVecIntSz num_comorbid = map.size();
  const VecVecIntSz num_visits = vcdb.size();

#ifdef ICD_DEBUG_SETUP
  Rcpp::Rcout << num_visits << " visits\n";
  Rcpp::Rcout << num_comorbid << " is num_comorbid\n";
#endif

  const ComorbidOut out = lookupComorbidByChunkFor(vcdb, map, chunk_size, omp_chunk_size);

#ifdef ICD_DEBUG
  Rcpp::Rcout << "out length is " << out.size() << "\n";
  // this next line now gives UBSAN in clang 3.7
  int outsum = std::accumulate(out.begin(), out.end(), 0);
  Rcpp::Rcout << "out sum is " << outsum << "\n";
  Rcpp::Rcout << "Ready to convert to R Matrix\n";
#endif
#ifdef ICD_DEBUG_TRACE
  Rcpp::Rcout << "out is: ";
  // printIt(out); // don't think I can do this through template, since char it is a base type?
  Rcpp::Rcout << "printed\n";
#endif
  // try cast to logical first. (in which case I can use char for Out)
  std::vector<bool> intermed;
  intermed.assign(out.begin(), out.end());
#ifdef ICD_DEBUG
  Rcpp::Rcout << "converted from ComorbidOut to vec bool, so Rcpp can handle cast to R logical vector\n";
#endif
  Rcpp::LogicalVector mat_out = Rcpp::wrap(intermed); // matrix is just a vector with dimensions (and col major...) Hope this isn't a data copy.
#ifdef ICD_DEBUG
  Rcpp::Rcout << "wrapped out\n";
#endif
  mat_out.attr("dim") = Rcpp::Dimension((int) num_comorbid, (int) num_visits); // set dimensions in reverse (row major for parallel step)
  mat_out.attr("dimnames") = Rcpp::List::create(icd9Mapping.names(),
               out_row_names);
  // apparently don't need to set class as matrix here
  Rcpp::Function t("t"); // use R transpose - seems pretty fast
#ifdef ICD_DEBUG
  Rcpp::Rcout << "Ready to transpose and return\n";
#endif
#ifdef ICD_VALGRIND
#ifdef ICD_DEBUG_TRACE
  Rcpp::Rcout << "Stopping valgrind instrumentation... ";
#endif
  CALLGRIND_STOP_INSTRUMENTATION;
  ;
  //CALLGRIND_DUMP_STATS;
#endif
  return t(mat_out);
}
