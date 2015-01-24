#' @title extract alphabetic, and numeric part of icd9 code prefix
#' @description removes whitespace and separates V or E if present.
#' @template icd9-any
#' @return vector or matrix, with first item of each row (or whole vector)
#'   containing V, E or "". The second part contains the numeric parts of the
#'   code, which may include a decimal point.
#' @keywords internal manip
icd9ExtractAlphaNumeric <- function(icd9) {
  # generate list, then flip into a matrix with a row for each code, and the
  # alpha part in first column, and numeric part in the second
  t(
    vapply(
      strMultiMatch(pattern = "([VvEe]?)([[:digit:].]+)",
                    text = as.character(icd9), dropEmpty = FALSE),
      function(x) matrix(data = x[1:2], nrow = 1, ncol = 2),
      FUN.VALUE = rep(NA_character_, times = 2)
    )
  )
}

#' @title drop zero padding from decimal ICD-9 code.
#' @description decimal form ICD-9 codes are not ambiguous if the leading zeroes
#'   are dropped. Some short-form ICD-9 codes would not be ambiguous, e.g. "1"
#'   but many would be problematic, so no function is provided to do this for
#'   short-form codes. This is the inverse of icd9AddLeadingZeroesDecimal
#' @template icd9-any
#' @return character vector of ICD-9 codes with extra zeroes dropped from major
#'   part
#' @family ICD-9 convert
#' @keywords internal manip
icd9DropLeadingZeroes <- function(icd9, isShort) {
  if (isShort) return(
    icd9DropLeadingZeroesShort(icd9Short = icd9))
  icd9DropLeadingZeroesDecimal(icd9Decimal = icd9)
}

#' @rdname icd9DropLeadingZeroes
#' @template icd9-decimal
icd9DropLeadingZeroesDecimal <- function(icd9Decimal) {

  out <- vapply(
    X = strMultiMatch(
      pattern = "[[:space:]]*([EeVv]?)(0*)([\\.[:digit:]]+)[[:space:]]*",
      text = icd9Decimal),
    FUN = function(x)
      if (length(x) > 0) sprintf("%s%s", x[1], x[3]) else NA_character_ ,
    FUN.VALUE = character(1) # template result PER vapply 'row'
  )
  out
}

#' @rdname icd9DropLeadingZeroes
#' @template icd9-short
icd9DropLeadingZeroesShort <- function(icd9Short) {
  parts <- icd9ShortToParts(icd9Short = icd9Short, minorEmpty = "")
  # very important: only drop the zero in V codes if the minor part is empty.
  areEmpty <- parts[["minor"]] == ""
  parts[areEmpty, "major"] <- icd9DropLeadingZeroesMajor(parts[areEmpty, "major"])
  icd9PartsToShort(parts)
}

#' @rdname icd9DropLeadingZeroes
icd9DropLeadingZeroesMajor <- function(major) {
  # trim everything??? TODO: consider just passing through the unprocessed
  # codes. remove trim tests?
  major <- trim(major)
  isV <- icd9IsV(major) #not checking validity, necessarily, just quick check
  # alternative might be just to get numeric-only, possibly quicker? TODO
  isN <- icd9IsValidMajorN(major)
  major[isV] <- sub(pattern = "^([Vv])0([[:digit:]]{1})$",
                                   replacement = "\\1\\2",
                                   x = major[isV])
  #just replace the FIRST string of zeros everything else is passed through
  major[isN] <- sub(pattern = "^[[:space:]]*0{1,2}",
                    replacement = "",
                    x = major[isN])
  major
}
