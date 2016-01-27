# Copyright (C) 2014 - 2015  Jack O. Wasey
#
# This file is part of icd9.
#
# icd9 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd9 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd9. If not, see <http:#www.gnu.org/licenses/>.

#' @title Check whether ICD-9 codes exist
#' @description This is different from syntactic validity: it looks it up in the
#'   canonical list of ICD-9 codes published by the CMS, and which are included
#'   in this package under \code{data-raw}. Checking syntactic validity using
#'   \code{link{icd9IsValid}} etc. is still useful, with a changing list of
#'   icd-9 codes over time, and possible imperfections in the master lists
#'   derived from CMS.
#' @param x vector if ICD codes to test whether defined in certain ICD code list
#' @template short_code
#' @param billable single logical value (default \code{FALSE}), if \code{TRUE}
#'   will divert to test whether the codes are in the billable list instead of
#'   seeing if they are any leaf or branch node. TODO: template
#' @template dotdotdot
#' @return logical vector
#' @export
icd_is_defined <- function(x, short_code = icd_guess_short(x), ...) {
  UseMethod("icd_is_defined")
}

#' @describeIn icd_is_defined Check whether ICD-9 code are defined. (ICD-9-CM by
#'   defauly)
#' @export
#' @keywords internal
icd_is_defined.icd9 <- function(x, short_code = icd_guess_short.icd9(x),
                                billable = FALSE, ...) {
  # assert(checkFactor(x), checkCharacter(x)) # what if icd10 class, but nothing else? be permissive?
  assertFlag(short_code)
  assertFlag(billable)

  if (!short_code)
    x <- icd_decimal_to_short.icd9(x)

  if (billable)
    icd9cm_is_billable.icd_short_code(asCharacterNoWarn(x))
  else
    icd9_add_leading_zeroes.icd_short_code(
      asCharacterNoWarn(x)) %in% icd9::icd9cm_hierarchy[["code"]]
}

#' @describeIn icd_is_defined Same for ICD-10-CM
#' @export
#' @keywords internal
icd_is_defined.icd10cm <- function(x, short_code = icd_guess_short.icd10(x),
                                   billable = FALSE, ...) {
  if (!short_code)
    x <- icd_decimal_to_short.icd10(x)

  if (billable)
    icd_is_billable.icd10cm(x, short_code = short_code)
  else
    x %fin% icd9::icd10cm2016[["code"]]
}

#' @describeIn icd_is_defined Same for ICD-10, temporarilyl using icd-10-cm for lookup
#' @export
#' @keywords internal
icd_is_defined.icd10 <- function(x, short_code = icd_guess_short.icd10(x),
                                 billable = FALSE, ...) {
  warning("using ICD-10-CM for testing icd10 definition. Set class to WHO if this is wanted.")
  icd_is_defined.icd10cm(x = x, short_code = short_code, billable = billable, ...)
}

# roxygen annotating this causes a roxygen error, issue #448
# https://github.com/klutometis/roxygen/issues/448
icd_is_defined.default <- function(x, short_code = icd_guess_short(x), ...) {
  # y <- icd_guess_version_update(x) %>% icd_guess_short_update(short_code = short_code)
  y <- icd_guess_version_update(x)
  UseMethod("icd_is_defined", y)
}

#' Select only defined ICD codes
#'
#' Return only those codes which are heading or leaf (billable), specifying
#' whether codes are all short-form or all decimal-form
#' @param x input vector or factor, possibly with an ICD class
#' @param short_code logical value, whether short-form ICD code
#' @param billable logical value, default \code{FALSE} whether any defined value, or, if \code{TRUE} only defined codes which are also considered billable, i.e. leaf nodes.
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
icd_get_defined.icd9 <- function(x, short_code = icd_guess_short.icd9(x), billable = FALSE) {
    x[icd_is_defined.icd9(x, short_code = short_code, billable = billable)]
}

#' @title Determine whether codes are billable leaf-nodes
#' @description Codes provided are compared to the most recent version of the
#'   CMS list of billable codes, or another version if specified.
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template short_code
#' @template invert
#' @param version single character string, default is "32" which is the latest
#'   release from CMS. Currently anything from "23" to "32" is accepted. Not
#'   numeric because there are possible cases with non-numeric names, e.g.
#'   revisions within one year, although none currently implemented.
#' @return logical vector of same length as input
#' @export
icd_is_billable <- function(...) {
  UseMethod("icd_is_billable")
}

#' @describeIn icd_is_billable Which of the given ICD-9 codes are leaf nodes in ICD-9-CM. Currently assumes ICD-9 codes are ICD-9-CM
#' @export
#' @keywords internal
icd_is_billable.icd9 <- function(x, short_code = icd_guess_short.icd9(x),
                                 version = icd9cm_latest_edition(), ...) {
  icd_is_billable.icd9cm(x = x, short_code = short_code, version = version)
}

#' @describeIn icd_is_billable Which of the given ICD-10 codes are leaf nodes in ICD-10-CM. Currently assumes ICD-10 codes are ICD-10-CM
#' @export
#' @keywords internal
icd_is_billable.icd10cm <- function(x, short_code = icd_guess_short.icd10(x),
                                    version = "2016", ...) {
  if (version != "2016")
    stop("curretly only ICD-10-CM 2016 version can be used to check for billable codes")
  if (!short_code)
    x <- icd_decimal_to_short.icd10(x)

  x %fin% icd10cm2016[icd10cm2016[["billable"]] == 1, "code"]
}

#' @describeIn icd_is_billable Which of the given ICD-10 codes are leaf nodes in ICD-10-CM. Currently assumes ICD-10 codes are ICD-10-CM
#' @export
#' @keywords internal
icd_is_billable.icd10 <- function(x, short_code = icd_guess_short.icd10(x),
                                 version = "2016", ...) {
  icd_is_billable.icd10cm(x = x, short_code = short_code, version = version)
}

#' @describeIn icd_is_billable Which of the given ICD-9 codes are leaf nodes in ICD-9-CM
#' @export
#' @keywords internal
icd_is_billable.icd9cm <- function(x, short_code = icd_guess_short(x),
                                   version = icd9cm_latest_edition(), ...) {
  assertVector(x)
  assertFlag(short_code)
  assertString(version)
  if (!short_code)
    x <- icd_decimal_to_short.icd9(x)
  x %in% icd9::icd9cm_billable[[version]][["icd9"]]
}

#' @describeIn icd_is_billable Which of the given ICD codes are leaf nodes in
#'   ICD version guessed from the codes themselves.
#' @export
#' @keywords internal
icd_is_billable.default <- function(x, short_code = icd_guess_short(x),
                                    version = icd9cm_latest_edition(), ...) {
  # guess ICD-9 vs ICD-10 and set class to dispatch again
  x <- icd_guess_version_update(x) %>% icd_guess_short_update(short_code = short_code)
  icd_is_billable(x)
}

# TODO: consider geteting rid of all these:

#' Check whether ICD-9 code(s) is/are billable
#'
#' Tests ICD-9 codes to see whether they are leaf nodes inthe ICD-9-CM hierarchy
#' @keywords internal
icd9cm_is_billable <- function(x, version = icd9cm_latest_edition()) {
  UseMethod("icd_is_billable")
}

#' @describeIn icd9cm_is_billable Are the given short-form codes leaf (billable)
#'   codes in the hierarchy?
icd9cm_is_billable.icd_short_code <- function(x, version = icd9cm_latest_edition())
  icd_is_billable.icd9(x, short_code = TRUE, version)

#' @describeIn icd9cm_is_billable Are the given decimal-form codes leaf (billable)
#'   codes in the hierarchy?
icd9cm_is_billable.icd_decimal_code <- function(x, version = icd9cm_latest_edition())
  icd_is_billable.icd9(x, short_code = FALSE, version)


#' @title Get billable ICD codes
#' @description Get billable ICD codes, implicitly, this refers to an ICD
#'   implementation which is specialized for a country, typically for billing,
#'   e.g. ICD-9-CM in the USA.
#' @param x input vector of ICD codes
#' @template short_code
#' @template invert
#' @param version e.g. "32", not ICD-9 vs ICD-10
#' @export
icd_get_billable <- function(...) {
  UseMethod("icd_get_billable")
}

#' @describeIn icd_get_billable Get billable ICD codes, guessing ICD version,
#'   and code short vs decimal type.
#' @export
#' @keywords internal
icd_get_billable.default <- function(x, ...) {
  x %>%
    icd_guess_version_update %>%
    icd_guess_short_update %>%
    icd_get_billable(...)
}

#' @describeIn icd_get_billable Get billable ICD-9-CM codes
#' @export
#' @keywords internal
icd_get_billable.icd9cm <- function(x, short_code = icd_guess_short.icd9(x),
                                    invert = FALSE, version = icd9cm_latest_edition(), ...) {
  assertFlag(short_code)
  assertFlag(invert)
  assertString(version)
  # would be nicer to dispatch on short_code type here.
  if (short_code)
    icd9cm_get_billable.icd_short_code(x = x, invert = invert, version = version)
  else
    icd9cm_get_billable.icd_decimal_code(x = x, invert = invert, version = version)
}

#' @describeIn icd_get_billable Get billable ICD-9 codes, which is currently implemented assuming ICD-9-CM
#' @export
#' @keywords internal
icd_get_billable.icd9 <- function(...) {
  icd_get_billable.icd9cm(...)
}

#' Get billable ICD-9-CM codes
#'
#' @description Return only those codes which are leaf codes in the hierarchy.
#'   In contrast to \emph{defined} codes, these are considered \emph{billable}
#'   and not super-sets of other codes. The majority have sub-decimal
#'   classifications, but some are simply three-digit codes. This particular
#'   function is currently restricted to ICD-9-CM codes.
#'
#' TODO: keep this internal
#' @param x data, e.g. character vector, \code{icd9}, \code{icd9cm}
#' @template short_code
#' @template invert
#' @param version String containing class name of version of ICD to be tested
#'   against, e.g. \code{"icd9cm"}. Default is to guess the version.
#' @keywords internal
icd9cm_get_billable <- function(x, short_code = icd_guess_short(x),
                                invert = FALSE, icd9cm_edition = icd9cm_latest_edition()) {
  assertVector(x)
  assertFlag(short_code)
  assertFlag(invert)
  assertString(version)
  UseMethod("icd9cm_get_billable")
}

#' @describeIn icd9cm_get_billable Get the billable ICD-9-CM codes from vector of short codes
#' @export
#' @keywords internal
icd9cm_get_billable.icd_short_code <- function(x, invert = FALSE, version = icd9cm_latest_edition()) {
  x[icd_is_billable.icd9cm(x, short_code = TRUE, version = version) != invert]
}

#' @describeIn icd9cm_get_billable Get the billable ICD-9-CM codes from vector of decimal codes
#' @export
#' @keywords internal
icd9cm_get_billable.icd_decimal_code <- function(x, invert = FALSE, version = icd9cm_latest_edition()) {
  x[icd_is_billable.icd9cm(x, short_code = FALSE, version = version) != invert]
}

