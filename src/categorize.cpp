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
#include "local.h"                             // for ICD_OPENMP
#include "config.h"                             // for ICD_VALGRIND
#include "util.h"
#include <stdlib.h>
#include <math.h>                              // for floor
#include <stdio.h>                             // for sprintf
#include <string.h>                            // for strcmp
#include <algorithm>                           // for copy, sort, transform
#include <iterator>                            // for back_insert_iterator
#include <ostream>                             // for size_t, operator<<
#include <string>                              // for string, basic_string
#include <vector>                              // for vector, vector<>::size...
#ifdef ICD_OPENMP
#include <omp.h>
#endif

using Rcpp::List;
using Rcpp::Vector;
using Rcpp::LogicalVector;
using Rcpp::IntegerVector;
using Rcpp::CharacterVector;
using Rcpp::DataFrame;
using Rcpp::String;
using Rcpp::Rcout;
using Rcpp::as;
using Rcpp::sugar::Max;
using Rcpp::Named;

Rcpp::List getEmptyDataFrame(String visit_name, String code_name) {
  TRACE("Making empty data frame.");
  List comorbid_df = List::create(

  );
  comorbid_df.attr("row.names") = CharacterVector::create();
  comorbid_df.attr("names") = CharacterVector::create(visit_name, code_name);
  comorbid_df.attr("class") = "data.frame";
  return comorbid_df;
}

List returnEmptyFrame(DataFrame df, String visit_name, String code_name) {
  DEBUG("There are no common codes between the map and patient data.");
  List out = List::create(
    Named("comorbid_df") = getEmptyDataFrame(visit_name, code_name),
    Named("unique_no_comorbid") = NA_LOGICAL
  );
  SEXP v = df[visit_name];
  switch (TYPEOF(v)) {
  case REALSXP: {
    Rcpp::NumericVector nv = v;
    out["unique_no_comorbid"] = unique(nv);
    return out;
  }
  case INTSXP: {
    CharacterVector l = (as<IntegerVector>(v)).attr("levels");
    bool isFactor = l.isNULL();
    Rcpp::NumericVector nv = v;
    if (isFactor) {
      Rcpp::Environment base("package:base");
      Rcpp::Function rUnique = base["unique.default"];
      out["unique_no_comorbid"] = rUnique(l);
      return out;
    }
    out["unique_no_comorbid"] = unique(nv);
    return out;
  }
  case STRSXP: {
    Rcpp::NumericVector nv = v;
    out["unique_no_comorbid"] = unique(nv);
    return out;
  }
  default: {
    Rcpp::stop("Unknown SEXP type");
  }
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
//' \dontrun{
//' R -e "devtools::load_all();devtools::test(filter='github')"
//' R -e "devtools::load_all();icd9_comorbid_ahrq(data.frame(a='vis', b='0010'))"
//' }
//' @md
//' @concept comorbidity comorbidities
//' @keywords internal manip
// [[Rcpp::export(factor_split_rcpp)]]
List factorSplit(const List &df,
                 const CharacterVector &relevant,
                 const String &id_name,
                 const String &code_name) {
  if (relevant.size() == 0) return returnEmptyFrame(df, id_name, code_name);
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
  IntegerVector new_level_idx = Rcpp::match(no_na_lx, relevant);
  DEBUG_VEC(new_level_idx);
  R_xlen_t fsz = x.size();
  DEBUG("fsz = " << fsz);
#ifdef ICD_OPENMP
  #pragma omp parallel for
#endif
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
  DEBUG_VEC(x);
  DEBUG_VEC(f_std);
  IntegerVector f = Rcpp::wrap(f_std);
  f.attr("levels") = relevant;
  f.attr("class") = "factor";
  CharacterVector all_visits_comorbid = visits[inc_mask];
  CharacterVector all_visits_no_comorbid =
    visits[is_na(match(visits, all_visits_comorbid))];
  assert((all(f > 0)).is_true());
  DEBUG(max(f));
  DEBUG(f.size());
  assert(max(f) < no_na_lx.size() + 1); // if relevant was correct, this should be =
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
  return List::create();
}
