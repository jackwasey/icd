#include "refactor.h"
#include "local.h"
#include <Rcpp.h>

using Rcpp::any;
using Rcpp::duplicated;
using Rcpp::match;
using Rcpp::rep;
using Rcpp::stop;

//' @title Factor without sorting \CRANpkg{Rcpp} implementation
//' @description Requiring character vector inputs only, no argument checking.
//' @keywords internal manip
//' @noRd
// [[Rcpp::export(factor_nosort_rcpp_worker)]]
Rcpp::IntegerVector factorNoSort(const Rcpp::CharacterVector& x,
                                 const Rcpp::CharacterVector& levels,
                                 const bool na_rm) {
  // Could re-purpose some of refactor to do this more quickly
  Rcpp::IntegerVector out = match(x, levels);
  out.attr("levels")      = (Rcpp::CharacterVector)levels;
  out.attr("class")       = "factor";
  if (!na_rm) return out;
  return out[!Rcpp::is_na(out)];
}

// SOMEDAY: can the following be done using the R_StringHash global cache
// instead of making a new hash table to do the integer matching?

//' @title Re-generate a factor with new levels, without doing string matching
//' @description This is called by an R wrapper. There is an \code{na.rm}
//'   version, too. Some work simply to mirror behavior of \code{base::factor},
//'   e.g. when a level is not available, but NA level is available, NA is
//'   inserted into the integer vector, not an index to the NA level.
//' @keywords internal manip
//' @noRd
// [[Rcpp::export(refactor_worker)]]
Rcpp::IntegerVector refactor(const Rcpp::IntegerVector& x,
                             const Rcpp::CharacterVector& new_levels,
                             const bool exclude_na,
                             const bool validate) {
  TRACE("Refactoring, keeping NAs");
  if (validate && !factorIsValid(x)) stop("input is not a valid factor");
  if (validate && is_true(any(duplicated(new_levels)))) {
    stop("With validation, refactor requires no duplicated levels");
  };
  Rcpp::IntegerVector f(x.size());
  Rcpp::CharacterVector lx = x.attr("levels");
  DEBUG_VEC(new_levels);
  Rcpp::CharacterVector no_na_new_levels;
  Rcpp::LogicalVector is_na_old_levels = is_na(lx);
  Rcpp::LogicalVector is_na_new_levels = is_na(new_levels);
  DEBUG_VEC(is_na_old_levels);
  DEBUG_VEC(is_na_new_levels);
  if (sum(is_na_new_levels) > 1) stop("New levels have multiple NA values, which is disallowed.");
  if (exclude_na) {
    DEBUG("Dropping NA in input levels");
    no_na_new_levels = new_levels[!is_na_new_levels];
  } else {
    DEBUG("Not dropping NA from target levels");
    no_na_new_levels = new_levels;
  }
  DEBUG_VEC(no_na_new_levels);
  if (no_na_new_levels.size() == 0) {
    DEBUG("no_na_new_levels is empty");
    f                = rep(NA_INTEGER, x.size());
    f.attr("levels") = Rcpp::CharacterVector::create();
    f.attr("class")  = "factor";
    return f;
  }
  // match is 1-indexed
  Rcpp::IntegerVector new_level_old_idx = match(lx, no_na_new_levels);
  DEBUG_VEC(new_level_old_idx);
  R_xlen_t fsz = x.size();
  DEBUG("fsz = " << fsz);
  Rcpp::LogicalVector matched_na_level(fsz, false);
  R_xlen_t i;
  bool new_na_level    = is_true(any(is_na_new_levels));
  bool old_na_level    = is_true(any(is_na_old_levels));
  int new_na_level_idx = which_max(is_na_new_levels);
  int old_na_level_idx = which_max(is_na_old_levels);
  for (i = 0; i < fsz; ++i) {
    TRACE("i=" << i << ", x[i]=" << x[i]);
    if (Rcpp::IntegerVector::is_na(x[i])) {
      if (!exclude_na && new_na_level) {
        TRACE("NA from pos " << i << " gets new NA level index");
        f[i] = new_na_level_idx + 1; // C to R index
        continue;
      }
      TRACE("NA from pos " << i << " stays as NA value");
      f[i] = NA_INTEGER;
      continue;
    }
    if (!exclude_na && new_na_level && old_na_level && (x[i] == old_na_level_idx + 1)) {
      TRACE("matched NA indexed in original levels");
      if (new_na_level) {
        TRACE("NA also in new levels, insert index to that NA");
        f[i] = new_na_level_idx + 1; // C to R inex
        continue;
      }
      TRACE("NA value");
      f[i] = NA_INTEGER;
      continue;
    }
    TRACE("x[i]  = " << x[i] << ", max is " << new_level_old_idx.size());
    assert(x[i] > 0);
    assert(x[i] <= new_level_old_idx.size());
    const auto cur = new_level_old_idx[x[i] - 1]; // cur is new R index into levels
    if (Rcpp::IntegerVector::is_na(cur)) {
      TRACE("fi++, leaving NA from " << i << " due to no match in new levels");
      f[i]                = NA_INTEGER;
      matched_na_level[i] = true;
    } else {
      TRACE("fi++, inserting " << cur << " from pos " << i);
      assert(cur > 0);
      assert(cur <= no_na_new_levels.size());
      f[i] = cur;
    }
  } // loop through input integer vector
  DEBUG_VEC(matched_na_level);
  DEBUG_VEC(f);
  f.attr("levels") = no_na_new_levels;
  f.attr("class")  = "factor";
  if (validate && !factorIsValid(f)) { stop("Refactor has failed: new factor is not valid."); }
  return f;
}

//' @describeIn refactor_worker Drop all \code{NA} values from levels and values
//' @keywords internal
//' @noRd
// [[Rcpp::export(refactor_narm_worker)]]
Rcpp::IntegerVector refactor_narm(const Rcpp::IntegerVector& x,
                                  const Rcpp::CharacterVector& new_levels,
                                  const bool validate) {
  TRACE("Refactoring, dropping NA");
  if (validate && !factorIsValid(x)) stop("input is not a valid factor");
  if (validate && is_true(any(duplicated(new_levels)))) {
    stop("With validation, refactor requires no duplicated levels");
  };
  Rcpp::IntegerVector f(x.size()); // too many if we are dropping NA values.
  f.attr("class")          = "factor";
  Rcpp::CharacterVector lx = x.attr("levels");
  DEBUG_VEC(new_levels);
  DEBUG("new_levels size: " << new_levels.size());
  Rcpp::CharacterVector no_na_lx;
  Rcpp::CharacterVector no_na_new_levels;
  // bool any_na_lx = false;
  Rcpp::LogicalVector is_na_new_levels = is_na(new_levels);
  Rcpp::LogicalVector is_na_old_levels = is_na(lx);
  DEBUG("Dropping NA in input factor levels");
  DEBUG_VEC(is_na_old_levels);
  DEBUG("Any old NA levels? " << is_true(any(is_na_old_levels)));
  no_na_lx = lx[!is_na_old_levels];
  DEBUG("Dropping NA in input levels");
  no_na_new_levels = new_levels[!is_na_new_levels];
  DEBUG("Any new NA levels? " << is_true(any(is_na_new_levels)));
  DEBUG_VEC(is_na_new_levels);
  DEBUG_VEC(no_na_lx);
  DEBUG_VEC(no_na_new_levels);
  if (no_na_new_levels.size() == 0) {
    DEBUG("no_na_new_levels is empty, so whole result must be empty");
    f                = Rcpp::IntegerVector::create();
    f.attr("levels") = Rcpp::CharacterVector::create();
    f.attr("class")  = "factor";
    return (f);
  }
  if (x.size() == 0) {
    DEBUG("x size is 0, so returning empty vector with no-NA levels.");
    f.attr("levels") = no_na_new_levels;
    return (f);
  }
  Rcpp::IntegerVector new_level_old_idx = match(no_na_lx, no_na_new_levels);
  DEBUG_VEC(new_level_old_idx);
  R_xlen_t fsz = x.size();
  DEBUG("fsz = " << fsz);
  R_xlen_t fi = 0;
  R_xlen_t i;
  for (i = 0; i < fsz; ++i) {
    TRACE("refactor_narm considering i: " << i << ", x[i]: " << x[i] << ", "
                                          << "fi: " << fi);
    if (Rcpp::IntegerVector::is_na(x[i])) {
      TRACE("dropping NA fom pos " << i << " (fi = " << fi << ") due to input NA value");
      continue;
    }
    assert(x[i] > 0);
    assert(x[i] <= is_na_old_levels.size()); // R index (to be used in C)
    if (is_na_old_levels[x[i] - 1]) {        // was an NA old level referenced?
      DEBUG("input data referenced an NA level"
            << "continuing without inserting from pos " << i << " with fi=" << fi);
      continue;
    }
    assert(x[i] > 0);
    assert(x[i] <= new_level_old_idx.size()); // R index
    // get new R index from C vec or R indices. Must keep as Rcpp::IntegerVector
    // length one, so NA is preserved.
    if (Rcpp::IntegerVector::is_na(new_level_old_idx[x[i] - 1])) {
      TRACE("dropping NA from " << i << " at pos " << fi << " due to no match with new levels");
    } else {
      int cur_i = new_level_old_idx[x[i] - 1];
      TRACE("inserting " << cur_i << " from pos i = " << i << " at fi = " << fi);
      assert(cur_i > 0);
      assert(cur_i <= no_na_new_levels.size());
      f[fi++] = cur_i;
    }
  } // for i
  DEBUG_VEC(f);
  f.attr("levels") = no_na_new_levels;
  f.attr("class")  = "factor";
  DEBUG("final fi = " << fi);
  if (fi == fsz) {
    if (validate) {
      if (validate && !factorIsValid(f)) { stop("Refactor has failed: new factor is not valid."); }
    }
    return (f);
  }
  DEBUG("copying f to shorten since NAs may have been dropped");
  Rcpp::IntegerVector f_no_na(f.begin(), f.begin() + fi);
  f_no_na.attr("levels") = no_na_new_levels;
  f_no_na.attr("class")  = "factor";
  if (validate && !factorIsValid(f_no_na)) {
    stop("Refactor has failed: new factor is not valid.");
  }
  return (f_no_na);
}

//' @title Check a factor structure is valid
//' @keywords internal
//' @noRd
// [[Rcpp::export(factor_is_valid)]]
bool factorIsValid(const Rcpp::IntegerVector& f) {
  DEBUG("factorIsValid?");
  DEBUG_VEC(f);
  DEBUG("max(f) " << max(f));
  if (!f.hasAttribute("levels")) return false;
  Rcpp::CharacterVector levels = f.attr("levels");
  DEBUG_VEC(levels);
  bool dupe_levels = is_true(any(duplicated(levels)));
  DEBUG("duplicated levels = " << dupe_levels);
  if (dupe_levels) return false;
  DEBUG("has class = " << f.hasAttribute("class"));
  if (!f.hasAttribute("class")) return false;
  DEBUG("any index too big = " << is_true(any(f > levels.size())));
  if (is_true(any(f > levels.size()))) return false;
  // Rf_isFactor currently just does:
  // return (TYPEOF(s) == INTSXP && inherits(s, "factor"));
  if (!Rf_isFactor(f)) return false;
  return true;
}
