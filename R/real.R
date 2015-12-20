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
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template short_code
#' @param onlyBillable single logical value (default \code{FALSE}), if
#'   \code{TRUE} will divert to test whether the codes are in the billable list
#'   instead of seeing if they are any leaf or branch node.
#' @return logical vector
#' @export
icd9IsReal <- function(icd9, isShort = icd_guess_short(icd9),
                       onlyBillable = FALSE) {
  if (isShort) return(icd9IsRealShort(icd9, onlyBillable))
  icd9IsRealDecimal(icd9, onlyBillable)
}

#' @describeIn icd9IsReal Are the given short-form codes defined at heading or
#'   leaf (billable) level?
#' @export
icd9IsRealShort <- function(x, onlyBillable = FALSE) {
  assert(checkFactor(x), checkCharacter(x))
  assertFlag(onlyBillable)
  if (onlyBillable)
    icd9cm_is_billable.icd_short_code(asCharacterNoWarn(x))
  else
    icd9_add_leading_zeroes.icd_short_code(asCharacterNoWarn(x)) %in% icd9::icd9_hierarchy[["icd9"]]
}

#' @describeIn icd9IsReal Are the given decimal-form codes defined at heading or
#'   leaf (billable) level?
#' @export
icd9IsRealDecimal <- function(x, onlyBillable = FALSE) {
  assert(checkFactor(x), checkCharacter(x))
  assertFlag(onlyBillable)
  if (onlyBillable)
    icd9cm_is_billable.icd_decimal_code(x)
  else
    icd9IsRealShort(icd_decimal_to_short.icd9(x))
}

#' @describeIn icd9IsReal Return only those codes which are heading or leaf
#'   (billable), specifying whether codes are all short-form or all decimal-form
#' @export
icd9GetReal <- function(x, isShort = icd_guess_short(x), onlyBillable = FALSE) {
  if (isShort)
    icd9GetRealShort(x)
  else
    icd9GetRealDecimal(x)
}

#' @describeIn icd9IsReal Return only those short-form codes which are heading
#'   or leaf (billable)
#' @export
icd9GetRealShort <- function(x, onlyBillable = FALSE)
  x[icd9IsRealShort(x, onlyBillable)]

#' @describeIn icd9IsReal Return only those decimal-form codes which are heading
#'   or leaf (billable)
#' @export
icd9GetRealDecimal <- function(x, onlyBillable = FALSE)
  x[icd9IsRealDecimal(x, onlyBillable)]

#' @title Determine whether codes are billable leaf-nodes
#' @description Codes provided are compared to the most recent version of the
#'   CMS list of billable codes, or another version if specified.
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template short_code
#' @param invert single logical value, if \code{TRUE}, then the non-billable
#'   codes are returned. For functions with logical result, just negate with
#'   \code{!}. Default is \code{FALSE}.
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
icd_is_billable.icd9 <- function(x, short_code = icd_guess_short(x),
                                 version = getLatestBillableVersion(), ...) {
  icd_is_billable.icd9cm(x = x, short_code = short_code, version = version)
}

#' @describeIn icd_is_billable Which of the given ICD-9 codes are leaf nodes in ICD-9-CM
icd_is_billable.icd9cm <- function(x, short_code = icd_guess_short(x),
                                   version = getLatestBillableVersion(), ...) {
  assertVector(x)
  assertFlag(short_code)
  assertString(version)
  if (!short_code)
    x <- icd_decimal_to_short.icd9(x)
  x %in% icd9::icd9cm_billable[[version]][["icd9"]]
}

icd_is_billable.default <- function(x, short_code = icd_guess_short(x),
                                    version = getLatestBillableVersion(), ...) {
  # guess ICD-9 vs ICD-10 and set class to dispatch again
  x = icd_guess_version_update(x)
  icd_is_billable(x)
}

# TODO: consider geteting rid of all these:

#' Check whether ICD-9 code(s) is/are billable
#'
#' Tests ICD-9 codes to see whether they are leaf nodes inthe ICD-9-CM hierarchy
#' @keywords internal
icd9cm_is_billable <- function(x, version = getLatestBillableVersion()) {
  UseMethod("icd_is_billable")
}

#' @describeIn icd9cm_is_billable Are the given short-form codes leaf (billable)
#'   codes in the hierarchy?
icd9cm_is_billable.icd_short_code <- function(x, version = getLatestBillableVersion())
  icd_is_billable.icd9(x, short_code = TRUE, version)

#' @describeIn icd9cm_is_billable Are the given decimal-form codes leaf (billable)
#'   codes in the hierarchy?
icd9cm_is_billable.icd_decimal_code <- function(x, version = getLatestBillableVersion())
  icd_is_billable.icd9(x, short_code = FALSE, version)


#' @title Get billable ICD codes
#' @description Get billable ICD codes, implicitly, this refers to an ICD
#'   implementation which is specialized for a country, typically for billing,
#'   e.g. ICD-9-CM in the USA.
#' @export
icd_get_billable <- function(...) {
  UseMethod("icd_get_billable")
}

#' @describeIn icd_get_billable Get billable ICD-9-CM codes
#' @export
icd_get_billable.icd9cm <- function(x, short_code = icd_guess_short(x),
                                    invert = FALSE, version = getLatestBillableVersion(), ...) {
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
icd_get_billable.icd9 <- function(...) {
  icd_get_billable.icd9cm(...)
}

#' @title Get billable ICD-9-CM codes
#' @description icd9_is_billable Return only those codes which are leaf (billable)
#'   codes in the hierarchy.
icd9cm_get_billable <- function(x, short_code = icd_guess_short(x),
                                invert = FALSE, version = getLatestBillableVersion()) {
  assertVector(x)
  assertFlag(short_code)
  assertFlag(invert)
  assertString(version)
  UseMethod("icd9cm_get_billable")
}

#' @describeIn icd9cm_get_billable Get the billable ICD-9-CM codes from vector of short codes
icd9cm_get_billable.icd_short_code <- function(x, invert = FALSE, version = getLatestBillableVersion()) {
  x[icd_is_billable.icd9(x, short_code = TRUE, version = version) != invert]
}

#' @describeIn icd9cm_get_billable Get the billable ICD-9-CM codes from vector of decimal codes
icd9cm_get_billable.icd_decimal_code <- function(x, invert = FALSE, version = getLatestBillableVersion()) {
  x[icd_is_billable.icd9(x, short_code = FALSE, version = version) != invert]
}

