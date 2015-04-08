
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

#' @describeIn icd9IsReal
#' @export
icd9IsRealShort <- function(icd9Short, onlyBillable = FALSE) {
  assertFactorOrCharacter(icd9Short)
  checkmate::assertFlag(onlyBillable)
  if (onlyBillable) return(icd9IsBillableShort(asCharacterNoWarn(icd9Short)))
  asCharacterNoWarn(icd9Short) %in% icd9::icd9Hierarchy[["icd9"]]
}

#' @describeIn icd9IsReal
#' @export
icd9IsRealDecimal <- function(icd9Decimal, onlyBillable = FALSE) {
  assertFactorOrCharacter(icd9Decimal)
  checkmate::assertFlag(onlyBillable)
  if (onlyBillable) return(icd9IsBillableDecimal(icd9Decimal))
  icd9IsRealShort(icd9DecimalToShort(icd9Decimal))
}

#' @describeIn icd9IsReal
#' @export
icd9GetReal <- function(icd9, isShort = icd9GuessIsShort(icd9), onlyBillable = FALSE) {
  if (isShort) return(icd9GetRealShort(icd9))
  icd9GetRealDecimal(icd9)
}

#' @describeIn icd9IsReal
#' @export
icd9GetRealShort <- function(icd9Short, onlyBillable = FALSE)
  icd9Short[icd9IsRealShort(icd9Short, onlyBillable)]

#' @describeIn icd9IsReal
#' @export
icd9GetRealDecimal <- function(icd9Decimal, onlyBillable = FALSE)
  icd9Decimal[icd9IsRealDecimal(icd9Decimal, onlyBillable)]


#' @title Determine whether codes are billable leaf-nodes
#' @description Codes provided are compared to the most recent version of the
#'   CMS list of billable codes, or another version if specified.
#' @template icd9-any
#' @template isShort
#' @param version single number describing the version of the CMS ICD-9-CM
#'   codes, defaults to the most recent available
#'   @return logical vector of same length as input
#' @export
icd9IsBillable <- function(icd9, isShort = icd9GuessIsShort(icd9),
                           version = getLatestBillableVersion()) {
  checkmate::assertVector(icd9)
  checkmate::assertFlag(isShort)
  checkmate::assertString(version)
  if (isShort)
    return(icd9 %in% icd9::icd9Billable[[version]][["icd9"]])
  icd9DecimalToShort(icd9) %in% icd9::icd9Billable[[version]][["icd9"]]
}

#' @describeIn icd9IsBillable
#' @export
icd9IsBillableShort <- function(icd9Short, version = getLatestBillableVersion())
  icd9IsBillable(icd9Short, isShort = TRUE, version)

#' @describeIn icd9IsBillable
#' @export
icd9IsBillableDecimal <- function(icd9Decimal, version = getLatestBillableVersion())
  icd9IsBillable(icd9Decimal, isShort = FALSE, version)

#' @describeIn icd9IsBillable
#' @export
icd9GetBillable <- function(icd9, isShort = icd9GuessIsShort(icd9),
                            invert = FALSE, version = getLatestBillableVersion()) {
  checkmate::assertVector(icd9)
  checkmate::assertFlag(isShort)
  checkmate::assertFlag(invert)
  checkmate::assertString(version)
  if (isShort)
    return(icd9[icd9IsBillableShort(icd9, version = version) != invert])
  icd9[icd9IsBillableDecimal(icd9, version = version) != invert]
}

#' @describeIn icd9IsBillable
#' @export
icd9GetBillableShort <- function(icd9Short, version = getLatestBillableVersion())
  icd9GetBillable(icd9Short, isShort = TRUE, version = version)

#' @describeIn icd9IsBillable
#' @export
icd9GetBillableDecimal <- function(icd9Decimal, version = getLatestBillableVersion())
  icd9GetBillable(icd9Decimal, isShort = FALSE, version = version)

#' @describeIn icd9IsBillable
#' @export
icd9GetNonBillableShort <- function(icd9Short, version = getLatestBillableVersion())
  icd9GetBillable(icd9Short, isShort = TRUE, invert = TRUE, version = version)

#' @describeIn icd9IsBillable
#' @export
icd9GetNonBillableDecimal <- function(icd9Decimal, version = getLatestBillableVersion())
  icd9GetBillable(icd9Decimal, isShort = FALSE, invert = TRUE, version = version)

#' @describeIn icd9IsBillable
#' @export
icd9GetNonBillable <- function(icd9, isShort = icd9GuessIsShort(icd9),
                               version = getLatestBillableVersion())
  icd9GetBillable(icd9, isShort = isShort, invert = TRUE, version = version)
