
#' @title warn or stop with invalid ICD-9 codes
#' @template icd9-any
#' @template short
#' @param callingFunction not implemented: ideally look at call stack and
#'   indicate who called here.
#' @keywords error
#' @family ICD9 validation
#' @export
stopIfInvalidIcd9 <- function(icd9, short, callingFunction="") {
  if (short && any(!icd9ValidShort(icd9)))
    stop("Invalid short-form ICD9 codes found: ", getInvalidShortICD9(icd9))
  if (!short && any(!icd9ValidDecimal(icd9))) 
    stop("Invalid long-form ICD9 codes found: ", getInvalidDecimalIcd9(icd9))
}

#' @rdname stopIfInvalidIcd9 
#' @export
warnIfInvalidIcd9 <- function(icd9, short, callingFunction="") {
  if (short && any(!icd9ValidShort(icd9)))
    warning("Invalid short-form ICD9 codes found: ", getInvalidShortICD9(icd9))
  if (!short && any(!icd9ValidDecimal(icd9))) 
    warning("Invalid long-form ICD9 codes found: ", getInvalidDecimalIcd9(icd9))
}

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
  
  if (class(icd9Decimal) != "character" & class(icd9Decimal) != 'factor') 
    stop("icd9ValidDecimal expects factor, character or numeric vector input but got class: ", class(icd9Decimal))
  if (length(icd9Decimal) == 0) stop("icd9ValidDecimal expects at least one icd9 code to test")
  
  # quick numeric check, although I think working purely in character would be
  # more reliable. e.g. by not introducing weird rounding errors using %%
  if (is.numeric(icd9Decimal)) {
    icd9Decimal <- as.numeric(icd9Decimal)
    return( # allow zero, which is 'no code' code.
      icd9Decimal >= 0 & icd9Decimal < 1000 & (icd9Decimal * 100 %% 1 == 0) 
    )
  }
  
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
  if (!(class(icd9Short) %in% c("character","factor"))) { 
    stop("isValidShortICD9 expects character vector input. Numeric is ambiguous, 
         so not allowed (although integers would not be ambiguous, simpler to stick to character-only.")
    # this is not just invalid data: there is a programming error in the data
    # structure
  }
  if (length(icd9Short) == 0) { 
    warning("isValidShortICD9 expects at least one icd9 code to test") 
    return() # return NULL, equivalent of c()
  }
  # quicker to test rather than always try to convert. Factor levels are always
  # character, so no concern about introducing ambiguity with e.g. short code of
  # 100 vs 10.0 (0100, 0010)
  if (class(icd9Short) =="factor") 
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
  grepl("^[[:space:]]*[Vv](([1-9][[:digit:]]?)|([[:digit:]][1-9]))(\\.[[:digit:]]{0,2})?[[:space:]]*$", icd9Decimal)

#' @rdname icd9ValidDecimal
#' @export
icd9ValidDecimalE <- function(icd9Decimal) 
  grepl("^[[:space:]]*[Ee][89][[:digit:]]{2}(\\.[[:digit:]]?)?[[:space:]]*$", icd9Decimal)

#' @rdname icd9ValidDecimal
#' @note TODO: icd9ValidDecimalN not quite right, since it would validate 0.12
#' @export
icd9ValidDecimalN <- function(icd9Decimal) 
  grepl("^[[:space:]]*((0{1,3})|([1-9][[:digit:]]{0,2})|(0[1-9][[:digit:]]?)|(00[1-9]))(\\.[[:digit:]]{0,2})?[[:space:]]*$", icd9Decimal) 

#' @title validate an icd9 mapping to comorbidities
#' @description just takes each item in each vector of the list of vectors and
#'   checks validity
#' @template mapping
#' @export
icd9ValidMappingShort <- function(icd9Mapping) all(unlist(lapply(icd9Mapping, FUN = icd9ValidShort), use.names=F))

#' @rdname icd9ValidMappingShort
#' @export
icd9ValidMappingDecimal <- function(icd9Mapping) all(unlist(lapply(icd9Mapping, FUN = icd9ValidDecimal), use.names=F))

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
getInvalidDecimalIcd9 <- function(icd9Decimal) icd9Decimal[!icd9ValidDecimal(icd9Decimal)]

#' @rdname getInvalidDecimalIcd9 
#' @template icd9-short
#' @export
getInvalidShortICD9 <- function(icd9Short) icd9Short[!icd9ValidShort(icd9Short)]
