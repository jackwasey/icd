#' @title warn or stop with invalid ICD-9 codes
#' @description In the case of warning, execution continues, and the logical
#'   vector containing the *invalid* codes is returned invisibly. Returning the
#'   invalids for consistency with the name of the function. The warn version of
#'   this function only uses one call to check validity, making execution as
#'   fast as possible.
#' @template icd9-any
#' @template short
#' @param callingFunction not implemented: ideally look at call stack and
#'   indicate who called here.
#' @keywords error
#' @family ICD9 validation
#' @export
stopIfInvalidIcd9 <- function(icd9, short, callingFunction = "") {
  if (short && any(!icd9ValidShort(icd9)))
    stop("Invalid short-form ICD9 codes found: ", paste(getInvalidShortIcd9(icd9)))
  if (!short && any(!icd9ValidDecimal(icd9)))
    stop("Invalid long-form ICD9 codes found: ", paste(getInvalidDecimalIcd9(icd9)))
}

#' @rdname stopIfInvalidIcd9
#' @export
warnIfInvalidIcd9 <- function(icd9, short, callingFunction = "") {
  invalid = FALSE
  if (short) {
    invalidLogical <- !icd9ValidShort(icd9)
  } else {
    invalidLogical <- !icd9ValidDecimal(icd9)
  }
  invalids <- icd9[invalidLogical] # duplicates the function getInvalid.... but avoids two calls to check validity.
  if (length(invalids > 0)) warning("Invalid ICD9 codes found: ", paste(invalids, collapse=", "))
  return(invisible(invalidLogical))
}

#' stop, warn, replace invalid with NA or continue
#'
#' Switch based on \code{invalidAction}
#'
#' @return icd9 codes, with invalid codes replaced by NA, if \code{invalidAction != "ignore"}
#' @template icd9-any
#' @template short
#' @template invalid
#' @family ICD9 validation
#' @keywords internal
icd9ValidNaWarnStop <- function(icd9, short, invalidAction = c("ignore", "silent", "warn", "stop")) {
  invalidAction <- match.arg(invalidAction)
  if (invalidAction == "ignore") return(invisible(icd9)) # get out quickly without expense of validation
  if (short) {
    valid <- icd9ValidShort(icd9)
  } else {
    valid <- icd9ValidDecimal(icd9)
  }
  if (any(!valid)) icd9WarnStopMessage("Invalid ICD9 codes found: ", paste(icd9[!valid]), invalidAction = invalidAction)
  icd9[!valid] <- NA # silent is only option left
  invisible(icd9)
}

#' @title warn or stop with message based on switch
#' @param ... message components passed on to \code{warning} or \code{stop}
#' @param invalid
#' @keywords internal
icd9WarnStopMessage <- function(..., invalidAction = c("stop", "warn", "silent")) {
  invalidAction <- match.arg(invalidAction)
  if (invalidAction == "warn") warning(...)
  if (invalidAction == "stop") stop(...)
}

#' @rdname icd9ValidNaWarnStop
icd9ValidNaWarnStopShort <- function(icd9, invalidAction)
  icd9ValidNaWarnStop(icd9 = icd9, short = TRUE, invalidAction = invalidAction)

#' @rdname icd9ValidNaWarnStop
icd9ValidNaWarnStopDecimal <- function(icd9, invalidAction)
  icd9ValidNaWarnStop(icd9 = icd9, short = FALSE, invalidAction = invalidAction)

#' @title check whether any ICD-9 code is syntactically valid
#' @template icd9-any
#' @template short
#' @seealso \code{\link{icd9ValidDecimal}} and \code{\link{icd9ValidShort}}
#' @family ICD9 validation
#' @export
icd9Valid <- function(icd9, short)
  if (short) icd9ValidShort(icd9) else icd9ValidDecimal(icd9)

#' @title check whether decimal icd9 codes are valid
#' @aliases icd9ValidDecimalN icd9ValidDecimalV icd9ValidDecimalE
#' @description Check validity of 'long' (i.e. decimal form) ICD9 codes. The
#'   codes may be numeric disease descriptiors or V or E prefixed.
#' @details Long form is not ambiguous so additional zeroes are tolerated.
#'   Although integer ICD-9 codes could be correct, there is a difference
#'   between 100 and 100.0, and indeed 100.00. Therefore, character class is
#'   enforced to avoid this problem.
#' @template icd9-decimal
#' @return logical vector with T or F for each icd9 code provided according to
#'   validity
#' @seealso \url{http://www.stata.com/users/wgould/icd9/icd9.hlp}
#'   url{http://www.sascommunity.org/wiki/Validate_the_format_of_ICD-9_codes}
#' @family ICD9 validation
#' @export
icd9ValidDecimal <- function(icd9Decimal) {

  if (length(icd9Decimal) == 0) {
    #warning("icd9ValidDecimal expects at least one code to test")
    return(logical())
  }
  if (class(icd9Decimal) != "character" & class(icd9Decimal) != 'factor')
    stop("icd9ValidDecimal expects factor or character vector input but got class: ",
         class(icd9Decimal), ". Numeric disallowed because of ambiguitities.")

  icd9ValidDecimalN(icd9Decimal) | icd9ValidDecimalV(icd9Decimal) | icd9ValidDecimalE(icd9Decimal)
}

#' @title validate ICD-9 short form code
#' @aliases icd9ValidShortN icd9ValidShortV icd9ValidShortE
#' @description As in \code{icd9ValidDecimal}, character type of the
#'   input is enforced. An integer 100 could correspond to decimal ICD-9 codes
#'   1.00 10.0 or 100.
#' @template icd9-short
#' @return logical vector with T or F for each icd9 code provided according to
#'   validity
#' @family ICD9 validation
#' @seealso \url{http://www.stata.com/users/wgould/icd9/icd9.hlp}
#'   \url{http://www.sascommunity.org/wiki/Validate_the_format_of_ICD-9_codes}
#' @export
icd9ValidShort <- function(icd9Short) {

  if (length(icd9Short) == 0) {
    #warning("isValidShortICD9 expects at least one icd9 code to test")
    return(logical())
  }

  if (!(class(icd9Short) %in% c("character", "factor"))) {
    stop("isValidShortICD9 expects character vector input. Numeric is
          ambiguous, so not allowed (although integers would not be ambiguous, simpler
          to stick to character-only.")
    # this is not just invalid data: there is a programming error in the data
    # structure
  }
  # quicker to test rather than always try to convert. Factor levels are always
  # character, so no concern about introducing ambiguity with e.g. short code of
  # 100 vs 10.0 (0100, 0010)
  if (class(icd9Short) == "factor")
    icd9Short <- asCharacterNoWarn(icd9Short)

  # as explained in details, a numeric short ID has different validity
  # requirements than a string because of leading zeroes.
  icd9ValidShortN(icd9Short) | icd9ValidShortV(icd9Short) | icd9ValidShortE(icd9Short)
}

#' @rdname icd9ValidShort
#' @export
icd9ValidShortV <- function(icd9Short)
  grepl("^[[:space:]]*[Vv](([1-9][[:digit:]]?)|([[:digit:]][1-9]))[[:digit:]]{0,2}[[:space:]]*$", icd9Short)

#' @rdname icd9ValidShort
#' @export
icd9ValidShortE <- function(icd9Short)
  grepl("^[[:space:]]*[Ee][89][[:digit:]]{2,3}[[:space:]]*$", icd9Short)

#' @rdname icd9ValidShort
#' @export
icd9ValidShortN <- function(icd9Short)
  grepl("^[[:space:]]*[[:digit:]]{1,5}[[:space:]]*$", icd9Short) # need to allow 0, but not 0.xx as valid code

#' @rdname icd9ValidDecimal
#' @export
icd9ValidDecimalV <- function(icd9Decimal)
  grepl("^[[:space:]]*[Vv](([1-9][[:digit:]]?)|([[:digit:]][1-9]))(\\.[[:digit:]]{0,2})?[[:space:]]*$",
        icd9Decimal)

#' @rdname icd9ValidDecimal
#' @export
icd9ValidDecimalE <- function(icd9Decimal)
  grepl("^[[:space:]]*[Ee][89][[:digit:]]{2}(\\.[[:digit:]]?)?[[:space:]]*$", icd9Decimal)

#' @rdname icd9ValidDecimal
#' @note TODO: icd9ValidDecimalN not quite right, since it would validate 0.12
#' @export
icd9ValidDecimalN <- function(icd9Decimal)
  grepl("^[[:space:]]*((0{1,3})|([1-9][[:digit:]]{0,2})|(0[1-9][[:digit:]]?)|(00[1-9]))(\\.[[:digit:]]{0,2})?[[:space:]]*$",
        icd9Decimal)

#' @title validate a major part
#' @description validation for just the 'major' part of an ICD-9 code. This can in fact be provided as a numeric, since there is no ambiguity. Numeric-only codes should be one to three digitis, V codes are followed by one or two digits, and E codes always by three digits between 800 and 999.
# example/test grepl(c("", "1", "22", "333", "4444", "V", "V2", "V34", "V567", "E", "E1", "E70", "E")
#' @template major
#' @family ICD9 validation
#' @export
icd9ValidMajor <- function(major) {
  grepl(
    pattern = "^[[:space:]]*([[:digit:]]{1,3}[[:space:]]*$)|([Vv][[:digit:]]{1,2}[[:space:]]*$)|([Ee][89][[:digit:]]{2}[[:space:]]*$)",
    x = major
  )
}

#' @title validate an icd9 mapping to comorbidities
#' @description just takes each item in each vector of the list of vectors and
#'   checks validity
#' @template mapping
#' @template short
#' @family ICD9 validation
#' @export
icd9ValidMapping <- function(icd9Mapping, short)
  all(unlist(
    lapply(icd9Mapping, FUN = function(icd9Map) icd9Valid(icd9Map, short)),
    use.names = FALSE
  ))

#' @rdname icd9ValidMapping
#' @export
icd9ValidMappingDecimal <- function(icd9Mapping) icd9ValidMapping(icd9Mapping, FALSE)

#' @rdname icd9ValidMapping
#' @export
icd9ValidMappingShort <- function(icd9Mapping) icd9ValidMapping(icd9Mapping, TRUE)

#' @rdname icd9ValidMapping
#' @export
icd9GetInvalidMappingShort <- function(icd9Mapping)
  unlist(lapply(icd9Mapping, FUN = getInvalidShortIcd9), use.names = FALSE)

#' @rdname icd9ValidMapping
#' @export
icd9GetInvalidMappingDecimal <- function(icd9Mapping)
  unlist(lapply(icd9Mapping, FUN = getInvalidDecimalIcd9), use.names = FALSE)

#' @title invalid subset of decimal or short ICD-9 codes
#' @aliases getInvalidShortIcd9
#' @description given vector of short or decimal ICD-9 codes in
#'   \code{icd9Decimal} or \code{icd9Short}, return (in the same format) those
#'   codes which are invalid. Useful for generating error messages with the
#'   faulty codes if validation fails.
#' @template icd9-decimal
#' @keywords manip
#' @family ICD-9 validation
#' @export
getInvalidDecimalIcd9 <- function(icd9Decimal) {
  icd9Decimal[!icd9ValidDecimal(icd9Decimal)]
}

#' @rdname getInvalidDecimalIcd9
#' @template icd9-short
#' @export
getInvalidShortIcd9 <- function(icd9Short) {
  icd9Short[!icd9ValidShort(icd9Short)]
}
