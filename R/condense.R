# Copyright (C) 2014 - 2016  Jack O. Wasey
#
# This file is part of icd.
#
# icd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd. If not, see <http:#www.gnu.org/licenses/>.

#' Condense ICD-9 code by replacing complete families with parent codes
#'
#' This can be thought of as the inverse operation to
#'   \code{icd9Children}.
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template short_code
#' @template onlyReal
#' @template dotdotdot
#' @family ICD-9 ranges
#' @export
icd_condense <- function(x, short_code = icd_guess_short(x), defined = NULL, warn = TRUE, ...) {
  UseMethod("icd_condense")
}

#' @export
#' @keywords internal
icd_condense.icd9 <- function(x, short_code = icd_guess_short(x), defined = NULL, warn = TRUE, ...) {
  assert(checkmate::checkFactor(x), checkmate::checkCharacter(x))
  assert_flag(short_code)
  assert(checkmate::checkNull(defined), checkmate::checkFlag(defined))
  assert_flag(warn)
  if (short_code)
    icd9_condense_short(x, defined = defined, warn = warn, ...)
  else
    icd9_condense_decimal(x, defined = defined, warn = warn, ...)
}

#' @describeIn icd_condense Condense a set of ICD codes, guessing ICD version
#'   from input data
#' @export
#' @keywords internal
icd_condense.character <- function(x, short_code = icd_guess_short(x), defined = NULL, ...) {

  guess <- icd_guess_version.character(x, short_code = short_code)
  if (guess == "icd9") {
    if (is.null(short_code)) short_code <- icd_guess_short(x)
    icd_condense.icd9(x, short_code = short_code, defined = defined, ...)
  } else if (guess == "icd10") {
    if (is.null(short_code)) short_code <- icd_guess_short(x)
    stop("icd_condense.icd10 not implemented yet")
  } else {
    stop("Unknown ICD type")
  }
}

#' @rdname icd_condense
#' @keywords internal manip
icd9_condense_decimal <- function(x, defined = NULL, warn = TRUE, keep_factor_levels = FALSE)
  icd_short_to_decimal.icd9(
    icd9_condense_short(
      icd_decimal_to_short.icd9(x), defined = defined, warn = warn, keep_factor_levels = keep_factor_levels))

#' @rdname icd_condense
#' @template warn
#' @param keep_factor_levels single logical value, default \code{FALSE}. If
#'   \code{TRUE}, will reuse the factor levels from the input data for the
#'   output data. This only applies if a factor is given for the input codes.
icd9_condense_short <- function(x, defined = NULL, warn = TRUE, keep_factor_levels = FALSE) {
  assert(checkmate::checkNull(defined), checkmate::checkFlag(defined))
  assert_flag(warn)
  assert_flag(keep_factor_levels)
  icd9Levels <- levels(x) # NULL if not a factor

  # we can convert back to factor later. Lots of scope for errors by handling
  # factors and character vectors in this function, so keep simple with
  # character only.
  x <- as_char_no_warn(x)
  i9w <- unique(icd_get_valid.icd9(x, short_code = TRUE))

  if (is.null(defined)) {
    if (all(icd_is_defined.icd9(i9w, short_code = TRUE))) {
      defined <- TRUE
      message("'defined' not given, but all codes are indeed defined, so assuming TRUE")
    } else {
      defined <- FALSE
      if (warn)
        warning("'defined' not given, but not all codes are defined so assuming FALSE")
    }
  }

  if (warn && defined && !all(icd_is_defined.icd9(x, short_code = TRUE))) {
    x <- icd_get_defined.icd9(x, short_code = TRUE)
    warning("only defined values requested, but some undefined ICD-9 code(s) were given, so dropping them")
  }

  # find good four digit parents for five digit codes
  # group with the provided four digit parents
  # find good three-digit parents for four digit codes
  # think about V and E codes.

  # any major codes are automatically in output (not condensing higher than
  # three digit code) and all their children can be removed from the work list
  out <- majors <- i9w[are_major <- icd_is_major.icd9(i9w)]
  i9w <- i9w[!are_major]
  i9w <- i9w[i9w %nin% icd_children.icd9(majors, short_code = TRUE, defined = defined)]
  fout <- c()
  four_digit_parents <- unique(substr(i9w, 0, 4))
  for (fp in four_digit_parents) {
    # onlyBillable at 5th level is same as onlyReal
    test_kids <- icd_children.icd9(fp, defined = defined, short_code = TRUE, billable = FALSE)
    if (length(test_kids) > 0 && all(test_kids %in% c(fp, i9w))) {
      #if ((length(test_kids) > 1) || (fp %in% i9w)) {
      fout <- c(fout, fp)
      i9w <- i9w[-which(i9w %in% test_kids)]
      #}
    }
  }

  # 'out' now has original major codes, 'fout' has parent four digit codes. Now
  # see whether any groups of four digit codes comprise all the four digit codes
  # in any parent (or original) major. We test the original majors again to
  # catch where non-billable four digit codes were omitted, but otherwise all
  # the children are complete. This replaces the old 'onlyBillable' flag which
  # was annoying.

  # set new variable so we don't change the thing we are looping over...
  major_parents <- unique(icd_get_major.icd9(c(out, fout, i9w), short_code = TRUE))
  for (mp in major_parents) {
    test_kids <- icd_children.icd9(mp, short_code = TRUE, defined = defined)
    test_kids <- test_kids[nchar(test_kids) < (5 + icd9_is_e(mp))] # we've done these already
    test_kids <- test_kids[-which(test_kids == mp)]
    if (length(test_kids) > 0 && all(test_kids %in% c(out, fout, i9w))) {
      out <- c(out, mp)
      fout <- fout[-which(fout %in% test_kids)]
    }
  }
  out <- unique(icd_sort.icd9(c(out, fout, i9w), short_code = TRUE))

  if (!is.null(icd9Levels)) {
    if (keep_factor_levels)
      out <- factor(out, icd9Levels)
    else
      out <- factor(out)
  }

  if (defined)
    icd_get_defined.icd9(out, short_code = TRUE) # would there be any undefined?
  else
    out
}
