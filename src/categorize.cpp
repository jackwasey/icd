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
#include "local.h"
#include "config.h"                             // for ICD_VALGRIND
#include "util.h"
#include <stdlib.h>
#include <math.h>                              // for floor
#include <stdio.h>
#include <string.h>                            // for strcmp
#include <algorithm>                           // for copy, sort, transform
#include <iterator>                            // for back_insert_iterator
#include <ostream>                             // for size_t, operator<<
#include <string>
#include <vector>
#ifdef ICD_OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

List returnEmptyFrame(DataFrame df, String id_name, String code_name) {
  DEBUG("No common codes between the map and patient data.");
  DEBUG("Returning empty data frame.");
  IntegerVector empty_factor = IntegerVector::create();
  empty_factor.attr("levels") = CV::create();
  empty_factor.attr("class") = "factor";
  DataFrame empty_df = DataFrame::create(
    Named(id_name) = CV::create(),
    Named(code_name) = empty_factor
  );
  SEXP v = df[id_name];
  List out = List::create(
    Named("comorbid_df") = empty_df,
    Named("unique_no_comorbid") = NA_LOGICAL
  );
  switch (TYPEOF(v)) {
  case REALSXP: {
    empty_df[id_name] = Rcpp::NumericVector();
    out["comorbid_df"] = empty_df;
    out["unique_no_comorbid"] = unique((NumericVector) v);
    return out;
  }
  case INTSXP: {
    empty_df[id_name] = IntegerVector();
    if (!Rf_isFactor(v))
      out["unique_no_comorbid"] = unique((IntegerVector) v);
    else {
      CV vlevs = ((IntegerVector) v).attr("levels");
      IntegerVector newids = empty_df[id_name];
      newids.attr("levels") = vlevs;
      out["unique_no_comorbid"] = vlevs;
    }
    out["comorbid_df"] = empty_df;
    return out;
  }
  case STRSXP: {
    out["unique_no_comorbid"] = unique((CV) v);
    return out;
  }
  default:
    Rcpp::stop("Unknown SEXP type when returning empty");
  }
}

//' @title Split data based on codes
//' @description Using C++ because I also want to quickly return the visits
//'   corresponding to the NA or non-NA factor elements. This is half-way to
//'   implementing the core comorbidity calculation all in C++.
//' @param df A `data.frame`, with a visit column (name given by `visit_name`),
//'   and a code column (name given by `code_name` -- not `icd_name` in this
//'   function). The code column must be a `factor`.
//' @param relevant A character vector containing the factor levels of interest.
//' @template visit_name
//' @param code_name String with name of column containing the codes.
//' @return Returns a list with two components:
//'   1. A reduced data frame, with only those rows where are least one visit
//'       had a relevant code
//'   2. A character vector with all the visits where there were no matching
//'       codes.
//' @examples
//'   df <- data.frame(visit_id = factor(c("visit1", "visit2", "visit1")),
//'                    icd_code = factor(c("410", "0010", "E999")))
//'   icd:::factor_split_rcpp(df, "410", "visit_id", "icd_code")
//'   icd:::factor_split_rcpp(df, "999", "visit_id", "icd_code")
//' \dontrun{
//' R -e "devtools::load_all();devtools::test(filter='github')"
//' R -e "devtools::load_all();icd9_comorbid_ahrq(data.frame(a='v', b='0010'))"
//' }
//' @md
//' @keywords internal manip
// [[Rcpp::export(factor_split_rcpp)]]
List factorSplit(const List &df,
                 const Nullable<CharacterVector> &relevant,
                 const String &id_name,
                 const String &code_name) {
  if (relevant.isNull())
    return returnEmptyFrame(df, id_name, code_name);
  CharacterVector rv(relevant);
  if (rv.size() == 0)
    return returnEmptyFrame(df, id_name, code_name);
  // TODO need to template over this for character/integer/factor visit ids?
  const CharacterVector visits = df[id_name];
  const IntegerVector &x = df[code_name];
  VecStr no_na_lx_std;
  VecInt f_std;
  f_std.reserve(x.size());
  LogicalVector inc_mask(x.size(), false);
  CharacterVector lx = x.attr("levels");
  no_na_lx_std.reserve(lx.size());
  DEBUG_VEC(x);
  DEBUG_VEC(lx);
  if (lx.isNULL()) Rcpp::stop("icd codes must be in a factor");
  for (auto l : lx) {
    if (l == NA_STRING) { // can't use is_na on a String
      DEBUG("Found NA in factor level");
      continue;
    }
    no_na_lx_std.push_back(as<std::string>(l));
  }
  DEBUG_VEC(no_na_lx_std);
  CV no_na_lx = Rcpp::wrap(no_na_lx_std);
  DEBUG_VEC(no_na_lx);
  DEBUG_VEC(relevant);
  IntegerVector new_level_idx = Rcpp::match(no_na_lx, rv);
  DEBUG_VEC(new_level_idx);
  R_xlen_t fsz = x.size();
  DEBUG("fsz = " << fsz);
  // Simply setting the loop parallel causes memory errors.
  //#ifdef ICD_OPENMP
  //  #pragma omp parallel for
  //#endif
  for (R_xlen_t i = 0; i < fsz; ++i) {
    TRACE("considering index x[i] - 1: " << x[i] - 1 << " from new_level_idx");
    if (IntegerVector::is_na(x[i])) {
      TRACE("leaving NA at pos " << i << "due to NA factor level in codes");
      continue;
    }
    auto cur = new_level_idx[x[i] - 1]; // get new R index with R index in C vec
    if (IntegerVector::is_na(cur)) {
      TRACE("leaving NA at pos " << i);
    } else {
      TRACE("inserting " << cur << " at pos " << i);
      inc_mask[i] = true;
      assert(cur > 0);
      assert(cur <= no_na_lx.size());
      f_std.push_back(cur);
    }
  }
  DEBUG_VEC(f_std);
  IntegerVector f = Rcpp::wrap(f_std);
  DEBUG_VEC(f);
  f.attr("levels") = rv;
  f.attr("class") = "factor";
  CharacterVector all_visits_comorbid = visits[inc_mask];
  CharacterVector all_visits_no_comorbid =
    visits[is_na(match(visits, all_visits_comorbid))];
  assert((all(f > 0)).is_true());
  DEBUG("max(f) =" << max(f));
  DEBUG("f.size() =" << f.size());
  DEBUG("no_na_lx.size() = " << no_na_lx.size());
  // this should be satisfied if relevant is correct:
  assert(f.size() == 0 || max(f) < (no_na_lx.size() + 1));
  List comorbid_df = List::create(Named(id_name) = all_visits_comorbid,
                                  Named(code_name) = f);
  Rcpp::CharacterVector rownames(f.size());
  char buffer[32];
  for (R_xlen_t i = 0; i != f.size(); ++i) {
    sprintf(buffer, "%lu", i);
    rownames(i) = buffer;
  }
  comorbid_df.attr("row.names") = rownames;
  comorbid_df.attr("names") = CharacterVector::create(id_name, code_name);
  comorbid_df.attr("class") = "data.frame";
  List out = List::create(
    Named("comorbid_df") = comorbid_df,
    Named("unique_no_comorbid") = unique(all_visits_no_comorbid));
  return out;
}

// [[Rcpp::export]]
List categorize_rcpp() {
  Rcpp::Rcerr << "Not implemented in pur C++ yet";
  return List::create();
}
