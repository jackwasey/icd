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
#'   in this package under \code{extdata}. Checking syntactic validity using
#'   \code{link{icd9IsValid}} etc. is still useful, with a changing list of
#'   icd-9 codes over time, and possible imperfections in the master lists
#'   derived from CMS.
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template isShort
#' @param onlyBillable single logical value (default \code{FALSE}), if
#'   \code{TRUE} will divert to test whether the codes are in the billable list
#'   instead of seeing if they are any leaf or branch node.
#' @return logical vector
#' @export
icd9IsReal <- function(icd9, isShort = icd9GuessIsShort(icd9),
                       onlyBillable = FALSE) {
  if (isShort) return(icd9IsRealShort(icd9, onlyBillable))
  icd9IsRealDecimal(icd9, onlyBillable)
}

#' @describeIn icd9IsReal Are the given short-form codes defined at heading or
#'   leaf (billable) level?
#' @export
icd9IsRealShort <- function(icd9Short, onlyBillable = FALSE) {
  assertFactorOrCharacter(icd9Short)
  assertFlag(onlyBillable)
  if (onlyBillable) return(icd9IsBillableShort(asCharacterNoWarn(icd9Short)))
  icd9AddLeadingZeroesShort(asCharacterNoWarn(icd9Short)) %in% icd9::icd9Hierarchy[["icd9"]]
}

#' @describeIn icd9IsReal Are the given decimal-form codes defined at heading or
#'   leaf (billable) level?
#' @export
icd9IsRealDecimal <- function(icd9Decimal, onlyBillable = FALSE) {
  assertFactorOrCharacter(icd9Decimal)
  assertFlag(onlyBillable)
  if (onlyBillable) return(icd9IsBillableDecimal(icd9Decimal))
  icd9IsRealShort(icd9DecimalToShort(icd9Decimal))
}

#' @describeIn icd9IsReal Return only those codes which are heading or leaf
#'   (billable), specifying whether codes are all short-form or all decimal-form
#' @export
icd9GetReal <- function(icd9, isShort = icd9GuessIsShort(icd9), onlyBillable = FALSE) {
  if (isShort) return(icd9GetRealShort(icd9))
  icd9GetRealDecimal(icd9)
}

#' @describeIn icd9IsReal Return only those short-form codes which are heading
#'   or leaf (billable)
#' @export
icd9GetRealShort <- function(icd9Short, onlyBillable = FALSE)
  icd9Short[icd9IsRealShort(icd9Short, onlyBillable)]

#' @describeIn icd9IsReal Return only those decimal-form codes which are heading
#'   or leaf (billable)
#' @export
icd9GetRealDecimal <- function(icd9Decimal, onlyBillable = FALSE)
  icd9Decimal[icd9IsRealDecimal(icd9Decimal, onlyBillable)]

#' @title Determine whether codes are billable leaf-nodes
#' @description Codes provided are compared to the most recent version of the
#'   CMS list of billable codes, or another version if specified.
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template isShort
#' @param invert single logical value, if \code{TRUE}, then the non-billable
#'   codes are returned. For functions with logical result, just negate with
#'   \code{!}. Default is \code{FALSE}.
#' @param version single character string, default is "32" which is the latest
#'   release from CMS. Currently anything from "23" to "32" is accepted. Not
#'   numeric because there are possible cases with non-numeric names, e.g.
#'   revisions within one year, although none currently implemented.
#' @return logical vector of same length as input
#' @export
icd9IsBillable <- function(icd9, isShort = icd9GuessIsShort(icd9),
                           version = getLatestBillableVersion()) {
  assertVector(icd9)
  assertFlag(isShort)
  assertString(version)
  if (isShort)
    return(icd9 %in% icd9::icd9Billable[[version]][["icd9"]])
  icd9DecimalToShort(icd9) %in% icd9::icd9Billable[[version]][["icd9"]]
}

#' @describeIn icd9IsBillable Are the given short-form codes leaf (billable)
#'   codes in the hierarchy?
#' @export
icd9IsBillableShort <- function(icd9Short, version = getLatestBillableVersion())
  icd9IsBillable(icd9Short, isShort = TRUE, version)

#' @describeIn icd9IsBillable Are the given decimal-form codes leaf (billable)
#'   codes in the hierarchy?
#' @export
icd9IsBillableDecimal <- function(icd9Decimal, version = getLatestBillableVersion())
  icd9IsBillable(icd9Decimal, isShort = FALSE, version)

#' @describeIn icd9IsBillable Return only those codes which are leaf (billable)
#'   codes in the hierarchy.
#' @export
icd9GetBillable <- function(icd9, isShort = icd9GuessIsShort(icd9),
                            invert = FALSE, version = getLatestBillableVersion()) {
  assertVector(icd9)
  assertFlag(isShort)
  assertFlag(invert)
  assertString(version)
  if (isShort)
    return(icd9[icd9IsBillableShort(icd9, version = version) != invert])
  icd9[icd9IsBillableDecimal(icd9, version = version) != invert]
}

#' @describeIn icd9IsBillable Return only those short-form codes which are leaf
#'   (billable) codes in the hierarchy.
#' @export
icd9GetBillableShort <- function(icd9Short, version = getLatestBillableVersion())
  icd9GetBillable(icd9Short, isShort = TRUE, version = version)

#' @describeIn icd9IsBillable Return only those decimal-form codes which are
#'   leaf (billable) codes in the hierarchy.
#' @export
icd9GetBillableDecimal <- function(icd9Decimal, version = getLatestBillableVersion())
  icd9GetBillable(icd9Decimal, isShort = FALSE, version = version)

#' @describeIn icd9IsBillable Return only those short-form codes which are not
#'   leaf (billable) codes in the hierarchy. This would include invalid and
#'   heading codes.
#' @export
icd9GetNonBillableShort <- function(icd9Short, version = getLatestBillableVersion())
  icd9GetBillable(icd9Short, isShort = TRUE, invert = TRUE, version = version)

#' @describeIn icd9IsBillable Return only those decimal-form codes which are not
#'   leaf (billable) codes in the hierarchy. This would include invalid and
#'   heading codes.
#' @export
icd9GetNonBillableDecimal <- function(icd9Decimal, version = getLatestBillableVersion())
  icd9GetBillable(icd9Decimal, isShort = FALSE, invert = TRUE, version = version)

#' @describeIn icd9IsBillable Return only those codes which are not leaf
#'   (billable) codes in the hierarchy. This would include invalid and heading
#'   codes. Codes are specified (or guessed) to be all decimal- or short-form.
#' @export
icd9GetNonBillable <- function(icd9, isShort = icd9GuessIsShort(icd9),
                               version = getLatestBillableVersion())
  icd9GetBillable(icd9, isShort = isShort, invert = TRUE, version = version)
