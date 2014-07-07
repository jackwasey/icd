
#' @title extract alphabetic, and numeric part of icd9 code
#'   prefix
#' @description removes whitespace and separates V or E if present.
#' @template icd9-any
#' @return vector or matrix, with first item of each row (or whole vector)
#'   containing V, E or "". The second part contains the numeric parts of the
#'   code, which may include a decimal point.
#' @keywords internal manip
icd9ExtractAlphaNumeric <- function(icd9) {
  # generate list, then flip into a matrix with a row for each code, and the alpha part in
  # first column, and numeric part in the second
  t(
    vapply(
      strMultiMatch(pattern = "([VvEe]?)([[:digit:].]+)", text = as.character(icd9), dropEmpty = FALSE),
      function(x) matrix(data = x[1:2], nrow = 1, ncol = 2),
      FUN.VALUE = rep(NA_character_, times = 2)
    )
  )
}

#' @title pad decimal ICD-9 codes with leading zeroes
#' @template icd9-any
#' @template isShort
#' @template addZeroV
#' @template invalid
#' @return character vector of icd9 codes
#' @keywords internal manip
icd9AddLeadingZeroes <- function(icd9, isShort, addZeroV = FALSE, invalidAction = icd9InvalidActions) {
  if (isShort) return(icd9AddLeadingZeroesShort(icd9Short = icd9, addZeroV = addZeroV, invalidAction = match.arg(invalidAction)))
  icd9AddLeadingZeroesDecimal(icd9Decimal = icd9, addZeroV = addZeroV, invalidAction = match.arg(invalidAction))
}

#' @rdname icd9AddLeadingZeroes
icd9AddLeadingZeroesDecimal <- function(icd9Decimal, addZeroV = FALSE, invalidAction = icd9InvalidActions) {
  invalidAction <- match.arg(invalidAction)
  #icd9Decimal <- icd9ValidNaWarnStopDecimal(icd9Decimal, invalidAction) # this is done in next step anyway.
  parts <- icd9DecimalToParts(icd9Decimal, invalidAction = invalidAction) # avoid infinite recursion by setting leadingZeroes to FALSE
  parts[["major"]] <- icd9AddLeadingZeroesMajor(parts[["major"]], addZeroV = addZeroV)
  # TODO: check strip whitespace from V & E? has this already been done by \code{parts}
  lzDecimal <- icd9PartsToDecimal(parts = parts)
  # if either part is 'NA', then return NA for that value
  lzDecimal[is.na(parts$major) | is.na(parts$minor)] <- NA
  # TODO: drop or retain the lone decimal?
  lzDecimal[parts$minor == ""] <- parts[parts$minor == "", "major"] # just pick major if minor was NA or "" (implies no lone decimal)
  lzDecimal
}

#' @rdname icd9AddLeadingZeroes
#' @description Non-decimal ICD-9 codes with length<5 are often ambiguous. E.g.
#'   100 could be 1.00 10.0 or 100
#' @template icd9-short
icd9AddLeadingZeroesShort <- function(icd9Short, addZeroV = FALSE, invalidAction = icd9InvalidActions) {
  invalidAction <- match.arg(invalidAction)
  icd9Short <- icd9ValidNaWarnStopShort(icd9Short, invalidAction)
  parts <- icd9ShortToParts(icd9Short, invalidAction = invalidAction)
  #already validated, so don't attempt further validation downstream
  parts[["major"]] <- icd9AddLeadingZeroesMajor(parts[["major"]], addZeroV = addZeroV, invalidAction = "ignore")
  out <- icd9PartsToShort(parts = parts)
  out[is.na(icd9Short)] <- NA_character_ # if we got NA in, then we give NA back.
  out
}

#' @rdname icd9AddLeadingZeroes
#' @description three digit codes are returned unchanged, one and two digit
#'   codes are preceded by 00 or 0. V codes are only zero padded if addZeroV is set to TRUE.
icd9AddLeadingZeroesMajor <- function(major, addZeroV = FALSE, invalidAction = icd9InvalidActions) {
  major <- icd9ValidNaWarnStopMajor(major = major, match.arg(invalidAction))
  isIntMajor <- areIntegers(major) # NA gives FALSE
  major[isIntMajor] <- sprintf("%03d", as.integer(major[isIntMajor]))
  major <- trim(major) # just because we always trim, and this is not necessarily done otherwise
  # if single digit V code, then try to slip in a zero. There are no valid E codes with E00, they start at 800.
  if (addZeroV) {
    justV <- icd9IsV(major)
    major[nchar(major[justV]) == 2] <- paste("V0", substr(major[justV], 2, 2), sep  = "")
  }
  major
}

#' @title drop zero padding from decimal ICD-9 code.
#' @description decimal form ICD-9 codes are not ambiguous if the leading zeroes
#'   are dropped. Some short-form ICD-9 codes would not be ambiguous, e.g. "1"
#'   but many would be problematic, so no function is provided to do this for
#'   short-form codes. This is the inverse of icd9AddLeadingZeroesDecimal
#' @template icd9-any
#' @template dropZeroV
#' @template invalid
#' @return character vector of ICD-9 codes with extra zeroes dropped from major
#'   part
#' @family ICD-9 convert
#' @keywords internal manip
icd9DropLeadingZeroes <- function(icd9, isShort, dropZeroV = FALSE, invalidAction = icd9InvalidActions) {
  if (isShort) return(icd9DropLeadingZeroesShort(icd9Short = icd9, dropZeroV = dropZeroV, invalidAction = match.arg(invalidAction)))
  icd9DropLeadingZeroesDecimal(icd9Decimal = icd9, dropZeroV = dropZeroV, invalidAction = match.arg(invalidAction))
}

#' @rdname icd9DropLeadingZeroes
#' @template icd9-decimal
icd9DropLeadingZeroesDecimal <- function(icd9Decimal, dropZeroV = FALSE, invalidAction = icd9InvalidActions) {
  icd9Decimal <- icd9ValidNaWarnStopDecimal(icd9Decimal, invalidAction = match.arg(invalidAction))
  # consider simplification in the vein of 'short' equivalent.

  out <- vapply(
    X = strMultiMatch(
      pattern = "[[:space:]]*([EeVv]?)(0*)([\\.[:digit:]]+)[[:space:]]*",
      text = icd9Decimal),
    FUN = function(x) { if (length(x) > 0) paste(x[[1]], x[[3]], sep = "") else NA_character_ },
    FUN.VALUE = character(1) # template result PER vapply 'row'
  )
  # if user request preservation of V0x, then keep the input data for those items.
  if (!dropZeroV) {
    isV <- icd9IsV(icd9Decimal)
    out[isV] <- icd9Decimal[isV]
  }
  out
}

#' @rdname icd9DropLeadingZeroes
#' @template icd9-short
icd9DropLeadingZeroesShort <- function(icd9Short, dropZeroV = FALSE, invalidAction = icd9InvalidActions) {
  # this may be a cheeky alternative: icd9DecimalToShort(icd9ShortToDecimal(icd9Short, leadingZeroes = FALSE, dropZeroV = dropZeroV))
  minorEmpty = ""
  parts <- icd9ShortToParts(icd9Short = icd9Short, minorEmpty = minorEmpty, invalidAction = match.arg(invalidAction))
  # very important: only drop the zero in V codes if the minor part is empty.
  if (!all(is.na(parts[["minor"]])))
    parts[parts[["minor"]] == minorEmpty, "major"] <- icd9DropLeadingZeroesMajor(
      parts[parts[["minor"]] == minorEmpty, "major"],
      dropZeroV = dropZeroV,
      invalidAction = "ignore"
    )
  icd9PartsToShort(parts = parts)
}

#' @rdname icd9DropLeadingZeroes
icd9DropLeadingZeroesMajor <- function(major, dropZeroV = FALSE, invalidAction = icd9InvalidActions) {
  stopifnot(is.logical(dropZeroV), length(dropZeroV) == 1, is.character(major) || is.numeric(major) || is.na(major))
  # trim everything??? TODO: consider just passing through the unprocessed codes. remove trim tests?
  major <- trim(icd9ValidNaWarnStopMajor(major, match.arg(invalidAction)))
  # E codes just pass through
  isV <- icd9IsV(major) #not checking validity, necessarily, just quick check
  isN <- icd9ValidMajorN(major) # alternative might be just to get numeric-only, possibly quicker? TODO
  if (dropZeroV) major[isV] <- sub(pattern = "^([Vv])0([[:digit:]]{1})$", replacement = "\\1\\2", x = major[isV])
  major[isN] <- sub(pattern = "^[[:space:]]*0{1,2}", replacement = "", x = major[isN]) # just replace the FIRST string of zeros
  #everything else is passed through
  major
}

#' @title get parent of an ICD-9 code
#' @description not likely to be useful for end-user, so using keyword
#'   \code{internal} for now.
#' @template minor
#' @template invalid
#' @return character vector of ICD-9 codes, one for each code given.
#' @keywords internal
icd9ParentMinor <- function(minor, invalidAction = icd9InvalidActions) {
  invalidAction <- match.arg(invalidAction)
  #todo - validate minors
  nOne <- nchar(minor) == 1
  minor[nOne] <- ""
  nTwo <- nchar(minor) == 2
  minor[nTwo] <- substr(minor[nTwo], 1, 1)
 minor
}

icd9Parent <- function(icd9, isShort, invalidAction = icd9InvalidActions) {

}
