
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
#' @param majorOk single logical, if \code{TRUE} will consider a three-digit
#'   code to be real, even though the majority of syntactically valid
#'   three-digit codes aren't billable.
#' @return logical vector
#' @export
icd9IsReal <- function(icd9, isShort = icd9GuessIsShort(icd9), majorOk = TRUE) {
  if (isShort) return(icd9IsRealShort(icd9, majorOk = majorOk))
  icd9IsRealDecimal(icd9, majorOk = majorOk)
}

#' @describeIn icd9IsReal
#' @export
icd9IsRealShort <- function(icd9Short, majorOk = TRUE) {
  assertFactorOrCharacter(icd9Short)
  icd9Short <- asCharacterNoWarn(icd9Short)
  checkmate::assertFlag(majorOk)
  if (majorOk)
    return(icd9Short %in% c(icd9::icd9Hierarchy[["icd9"]],
                            icd9::icd9ChaptersMajor))
  icd9Short %in% icd9::icd9Hierarchy[["icd9"]]
}

#' @describeIn icd9IsReal
#' @export
icd9IsRealDecimal <- function(icd9Decimal, majorOk = TRUE) {
  icd9IsRealShort(icd9DecimalToShort(icd9Decimal), majorOk = majorOk)
}

#' @describeIn icd9IsReal
#' @export
icd9GetReal <- function(icd9, isShort = icd9GuessIsShort(icd9), majorOk = TRUE) {
  if (isShort) return(icd9GetRealShort(icd9, majorOk = majorOk))
  icd9GetRealDecimal(icd9, majorOk = majorOk)
}

#' @describeIn icd9IsReal
#' @export
icd9GetRealShort <- function(icd9Short, majorOk = TRUE)
  icd9Short[icd9IsRealShort(icd9Short, majorOk = majorOk)]

#' @describeIn icd9IsReal
#' @export
icd9GetRealDecimal <- function(icd9Decimal, majorOk = TRUE)
  icd9Decimal[icd9IsRealDecimal(icd9Decimal, majorOk = majorOk)]


#' @describeIn icd9IsReal
#' @export
icd9IsReallyReal <- function(icd9, isShort = icd9GuessIsShort(icd9)) {

}
