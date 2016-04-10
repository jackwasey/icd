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

utils::globalVariables("icd10cm2016")

#' Check whether ICD-9 codes exist
#'
#' This is different from syntactic validity: it looks it up in the canonical
#' list of ICD-9 codes published by the CMS, and which are included in this
#' package under \code{data-raw}. Checking syntactic validity using
#' \code{link{icd_is_valid}} etc. is still useful, with a changing list of ICD-9
#' codes over time, and possible imperfections in the master lists derived from
#' CMS.
#' @param x vector if ICD codes to test whether defined in certain ICD code list
#' @template short_code
#' @template billable
#' @template dotdotdot
#' @return logical vector
#' @export
icd_is_defined <- function(x, short_code = icd_guess_short(x), ...) {
  UseMethod("icd_is_defined")
}

#' @describeIn icd_is_defined Check whether ICD-9 code are defined. (ICD-9-CM by
#'   default)
#' @export
#' @keywords internal
icd_is_defined.icd9 <- function(x, short_code = icd_guess_short(x),
                                billable = FALSE, ...) {
  assert(checkmate::checkCharacter(x), checkmate::checkFactor(x))
  assert_flag(short_code)
  assert_flag(billable)

  if (!short_code)
    x <- icd_decimal_to_short.icd9(x)

  if (billable)
    icd_is_billable.icd9cm(short_code = TRUE, as_char_no_warn(x))
  else
    icd9_add_leading_zeroes(short_code = TRUE, as_char_no_warn(x)) %in%
    icd::icd9cm_hierarchy[["code"]]
}

#' @describeIn icd_is_defined Same for ICD-10-CM
#' @param nomatch integer value, passed to \code{fmatch} default is 0. Setting
#'   this to \code{NA_integer_} would stop NA values being treated as undefined.
#' @export
#' @keywords internal
icd_is_defined.icd10cm <- function(x, short_code = icd_guess_short(x),
                                   billable = FALSE, nomatch = 0L, ...) {
  assert(checkmate::checkCharacter(x), checkmate::checkFactor(x))
  if (!short_code)
    x <- icd_decimal_to_short(x)

  if (billable)
    icd_is_billable.icd10cm(x, short_code = short_code)
  else
    fmatch(x, icd::icd10cm2016[["code"]], nomatch = nomatch, ...) > 0L
}

#' @describeIn icd_is_defined Same for ICD-10, temporarily using ICD-10-CM until
#'   ICD-10 WHO is available in this package
#' @export
#' @keywords internal
icd_is_defined.icd10 <- function(x, short_code = icd_guess_short(x),
                                 billable = FALSE, ...) {
  icd_is_defined.icd10cm(x = x, short_code = short_code, billable = billable, ...)
}

#' @describeIn icd_is_defined default method which will guess the ICD version (9
#'   vs 10, maybe WHO vs CM or other in the future) and dispatches again on that
#'   type.
#' @export
#' @keywords internal
icd_is_defined.default <- function(x, short_code = icd_guess_short(x), ...) {
  switch(
    icd_guess_version(x, short_code = short_code),
    "icd9" = icd_is_defined.icd9(x, short_code = short_code, ...),
    "icd10" = icd_is_defined.icd10(x, short_code = short_code, ...),
    stop("ICD version not known")
  )
}

#' Select only defined ICD codes
#'
#' Return only those codes which are heading or leaf (billable), specifying
#' whether codes are all short-form or all decimal-form
#' @param x input vector or factor, possibly with an ICD class
#' @param short_code logical value, whether short-form ICD code
#' @template billable
#' @export
icd_get_defined <- function(x, short_code = icd_guess_short(x), billable = FALSE) {
  UseMethod("icd_get_defined")
}

#' @export
#' @keywords internal
icd_get_defined.default <- function(x, short_code = icd_guess_short(x), ...) {
  icd_ver <- icd_guess_version(x)
  if (icd_ver != "icd9")
    stop("testing whether ICD codes are defined is currently only implemented for ICD-9-CM")
  x[icd_is_defined.icd9(x, short_code, ...)]
}

#' @export
#' @keywords internal
icd_get_defined.icd9 <- function(x, short_code = icd_guess_short(x), billable = FALSE) {
  x[icd_is_defined.icd9(x, short_code = short_code, billable = billable)]
}

#' Determine whether codes are billable leaf-nodes
#'
#' Codes provided are compared to the most recent edition of the CMS list of
#' billable codes, or another edition if specified.
#' @param x input vector to test
#' @template short_code
#' @param icd9cm_edition single character string, default is "32" which is the
#'   latest release from CMS. Currently anything from "23" to "32" is accepted.
#'   Not numeric because there are possible cases with non-numeric names, e.g.
#'   revisions within one year, although none currently implemented.
#' @param icd10cm_edition single character string. ICD-10-CM editions are
#'   currently just the year of release.
#' @return logical vector of same length as input
#' @export
icd_is_billable <- function(x, short_code = icd_guess_short(x), ...) {
  UseMethod("icd_is_billable")
}

#' @describeIn icd_is_billable Which of the given ICD-9 codes are leaf nodes in
#'   ICD-9-CM. Currently assumes ICD-9 codes are ICD-9-CM
#' @export
#' @keywords internal
icd_is_billable.icd9 <- function(x, short_code = icd_guess_short(x),
                                 icd9cm_edition = icd9cm_latest_edition(), ...) {
  icd_is_billable.icd9cm(x = x, short_code = short_code, icd9cm_edition = icd9cm_edition)
}

#' @describeIn icd_is_billable Which of the given ICD-10 codes are leaf nodes in
#'   ICD-10-CM. Currently assumes ICD-10 codes are ICD-10-CM
#' @param icd10cm_edition single character string. ICD-10-CM editions are
#'   currently just the year of release.
#' @export
#' @keywords internal
icd_is_billable.icd10cm <- function(x, short_code = icd_guess_short(x), icd10cm_edition = "2016", ...) {
  if (icd10cm_edition != "2016")
    stop("curretly only ICD-10-CM 2016 edition can be used to check for billable codes")
  if (!short_code)
    x <- icd_decimal_to_short(x)

  x %fin% icd10cm2016[icd10cm2016[["billable"]] == 1, "code"]
}

#' @describeIn icd_is_billable Which of the given ICD-10 codes are leaf nodes in
#'   ICD-10-CM. Currently assumes ICD-10 codes are ICD-10-CM
#' @export
#' @keywords internal
icd_is_billable.icd10 <- function(x, short_code = icd_guess_short(x), icd10cm_edition = "2016", ...) {
  icd_is_billable.icd10cm(x = x, short_code = short_code, icd10cm_edition = icd10cm_edition)
}

#' @describeIn icd_is_billable Which of the given ICD-9 codes are leaf nodes in
#'   ICD-9-CM
#' @param nomatch integer value, passed to \code{match} default is 0.
#' @export
#' @keywords internal
icd_is_billable.icd9cm <- function(x, short_code = icd_guess_short(x),
                                   icd9cm_edition = icd9cm_latest_edition(), nomatch = 0L, ...) {
  assert_vector(x)
  assert_flag(short_code)
  assert_string(icd9cm_edition)
  if (!short_code)
    x <- icd_decimal_to_short.icd9(x)
  fmatch(x, icd::icd9cm_billable[[icd9cm_edition]][["code"]], nomatch = nomatch, ...) > 0L
}

#' @describeIn icd_is_billable Which of the given ICD codes are leaf nodes in
#'   ICD version (9 vs 10) guessed from the codes themselves.
#' @export
#' @keywords internal
icd_is_billable.default <- function(x, short_code = icd_guess_short(x), ...) {
  switch(
    icd_guess_version(x, short_code = short_code),
    "icd9" = icd_is_billable.icd9(x, short_code = short_code, ...),
    "icd10" = icd_is_billable.icd10(x, short_code = short_code, ...),
    stop("Unknown ICD version.")
  )
}

#' Get billable ICD codes
#'
#' Get billable ICD codes, implicitly, this refers to an ICD
#'   implementation which is specialized for a country, typically for billing,
#'   e.g. ICD-9-CM in the USA.
#' @param x input vector of ICD codes
#' @template short_code
#' @template invert
#' @param icd9cm_edition e.g. "32", not ICD-9 vs ICD-10
#' @export
icd_get_billable <- function(...) {
  UseMethod("icd_get_billable")
}

#' @describeIn icd_get_billable Get billable ICD codes, guessing whether ICD-9
#'   or ICD-10, and code short vs decimal type.
#' @export
#' @keywords internal
icd_get_billable.default <- function(x, short_code = icd_guess_short(x), ...) {
  switch(
    icd_guess_version(x, short_code = short_code),
    "icd9" = icd_get_billable.icd9(x, short_code = short_code, ...),
    "icd10" = icd_get_billable.icd10(x, short_code = short_code, ...),
    stop("Unknown ICD version.")
  )
}

#' @describeIn icd_get_billable Get billable ICD-9-CM codes
#' @export
#' @keywords internal
icd_get_billable.icd9cm <- function(x, short_code = icd_guess_short(x),
                                    invert = FALSE, icd9cm_edition = icd9cm_latest_edition(), ...) {
  assert_flag(short_code)
  assert_flag(invert)
  assert_string(icd9cm_edition)
  # would be nicer to dispatch on short_code type here.
  icd9cm_get_billable(x = x, short_code = short_code, invert = invert, icd9cm_edition = icd9cm_edition)
}

#' @describeIn icd_get_billable Get billable ICD-9 codes, which is currently
#'   implemented assuming ICD-9-CM
#' @export
#' @keywords internal
icd_get_billable.icd9 <- function(...) {
  icd_get_billable.icd9cm(...)
}

#' Get billable ICD-9-CM codes
#'
#' Return only those codes which are leaf codes in the hierarchy. In contrast to
#' \emph{defined} codes, these are considered \emph{billable} and not super-sets
#' of other codes. The majority have sub-decimal classifications, but some are
#' simply three-digit codes. This particular function is currently restricted to
#' ICD-9-CM codes.
#'
#' @param x data, e.g. character vector, \code{icd9}, \code{icd9cm}
#' @template short_code
#' @template invert
#' @keywords internal
icd9cm_get_billable <- function(x, short_code = icd_guess_short(x),
                                invert = FALSE, icd9cm_edition = icd9cm_latest_edition()) {
  assert_vector(x)
  assert_flag(short_code)
  assert_flag(invert)
  assert_string(icd9cm_edition)
  x <- as.icd_short_diag(as.icd9cm(x), short_code)

  x[icd_is_billable.icd9cm(x, short_code = short_code, icd9cm_edition = icd9cm_edition) != invert]
}

#' @describeIn icd_get_billable Get billable, i.e. leaf nodes from ICD-10-CM
#' @export
#' @keywords internal
icd_get_billable.icd10cm <- function(x, short_code = icd_guess_short(x),
                                     invert = FALSE, icd10cm_edition = "2016", ...) {
  assert_vector(x)
  assert_flag(short_code)
  assert_flag(invert)
  assert_string(icd10cm_edition)
  x <- as.icd_short_diag(as.icd10cm(x), short_code)
  x[icd_is_billable.icd10cm(x, short_code = short_code, icd10cm_edition = icd10cm_edition) != invert]
}

#' @describeIn icd_get_billable Get billable, i.e. leaf nodes from ICD-10-CM
#' @export
#' @keywords internal
icd_get_billable.icd10 <- function(x, short_code = icd_guess_short(x),
                                   invert = FALSE, icd10cm_edition = "2016", ...) {
  icd_get_billable.icd10cm(x = x, short_code = short_code, invert = invert, icd10cm_edition = icd10cm_edition)
}
