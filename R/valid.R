
#' @title stop or warn, then  log if any of given ICD9 codes is invalid
#' @template icd9
#' @template short
#' @param callingFunction not implemented: ideally look at call stack and indicate who called here.
#' @export
stopIfInvalidICD9 <- function(icd9codes, short, callingFunction="") {
  if (short && any(!icd9ValidShort(icd9codes)))
    stop("Invalid short-form ICD9 codes found: ", getInvalidShortICD9(icd9codes))
  if (!short && any(!icd9ValidDecimal(icd9codes))) 
    stop("Invalid long-form ICD9 codes found: ", getInvalidDecimalIcd9(icd9codes))
}

#' @describeIn stopIfInvalidICD9
#' @export
warnIfInvalidICD9 <- function(icd9codes, short, callingFunction="") {
  if (short && any(!icd9ValidShort(icd9codes)))
    warning("Invalid short-form ICD9 codes found: ", getInvalidShortICD9(icd9codes))
  if (!short && any(!icd9ValidDecimal(icd9codes))) 
    warning("Invalid long-form ICD9 codes found: ", getInvalidDecimalIcd9(icd9codes))
}

#' @title check whether icd9 codes are valid
#' @description Check validity of short or 'long' (i.e. decimal form) ICD9 codes. 
#' The codes may be numeric disease descriptiors or V or E prefixed for procedures or ?complications.
#' @details Long form is not ambiguous. However, the decimal-free form is stored
#' in the Hopkins database. The numbers are written as characters so the
#' essential preceding zeroes are included. This means a non-decimal ICD9 code,
#' like 1000, is ambiguous and should make an error.
#' @template icd9
#' @return logical vector with T or F for each icd9 code provided according to validity
#' @seealso http://www.stata.com/users/wgould/icd9/icd9.hlp and http://www.sascommunity.org/wiki/Validate_the_format_of_ICD-9_codes
#' @export
icd9ValidDecimal <- function(icd9) {
  
  if (class(icd9) != "character" & class(icd9) != 'factor') 
    stop("icd9ValidDecimal expects factor, character or numeric vector input but got class: ", class(icd9))
  if (length(icd9)==0) stop("icd9ValidDecimal expects at least one icd9 code to test")
  
  # quick numeric check, although I think working purely in character would be
  # more reliable. e.g. by not introducing weird rounding errors using %%
  if (is.numeric(icd9)) {
    icd9 <- as.numeric(icd9)
    return(
      icd9>=0 & icd9<1000 & (icd9*100%%1 == 0) # allow zero, which is 'no code' code.
    )
  }
  
  icd9ValidDecimalN(icd9) | icd9ValidDecimalV(icd9) | icd9ValidDecimalE(icd9)
}

#' @describeIn icd9ValidDecimal
#' @export
icd9ValidShort <- function(icd9) {
  if (!(class(icd9) %in% c("character","factor"))) { 
    stop("isValidShortICD9 expects character vector input. Numeric is ambiguous, 
         so not allowed (although integers would not be ambiguous, simpler to stick to character-only.")
    # this is not just invalid data: there is a programming error in the data structure
  }
  if (length(icd9) == 0) { 
    warning("isValidShortICD9 expects at least one icd9 code to test") 
    return() # return NULL, equivalent of c()
  }
  if (class(icd9) =="factor") # quicker to test rather than always try to convert
    icd9 <- asCharacterNoWarn(icd9) # factor levels are always character, so no concern about introducing ambiguity with e.g. short code of 100 vs 10.0 (0100, 0010)
  
  # as explained in details, a numeric short ID has different validity requirements than a string because of leading zeroes.
  icd9ValidShortN(icd9) | icd9ValidShortV(icd9) | icd9ValidShortE(icd9)
}

#' @describeIn icd9ValidDecimal
#' @export
icd9ValidShortV <- function(icd9) grepl("^[[:space:]]*[Vv](([1-9][[:digit:]]?)|([[:digit:]][1-9]))[[:digit:]]{0,2}[[:space:]]*$", icd9)

#' @describeIn icd9ValidDecimal
#' @export
icd9ValidShortE <- function(icd9) grepl("^[[:space:]]*[Ee][89][[:digit:]]{2,3}[[:space:]]*$", icd9)

#' @describeIn icd9ValidDecimal
#' @export
icd9ValidShortN <- function(icd9) grepl("^[[:space:]]*[[:digit:]]{1,5}[[:space:]]*$", icd9) # need to allow 0, but not 0.xx as valid code

#' @describeIn icd9ValidDecimal
#' @export
icd9ValidDecimalV <- function(icd9) grepl("^[[:space:]]*[Vv](([1-9][[:digit:]]?)|([[:digit:]][1-9]))(\\.[[:digit:]]{0,2})?[[:space:]]*$", icd9)

#' @describeIn icd9ValidDecimal
#' @export
icd9ValidDecimalE <- function(icd9) grepl("^[[:space:]]*[Ee][89][[:digit:]]{2}(\\.[[:digit:]]?)?[[:space:]]*$", icd9)

#' @describeIn icd9ValidDecimal
#' @export
icd9ValidDecimalN <- function(icd9) grepl("^[[:space:]]*((0{1,3})|([1-9][[:digit:]]{0,2})|(0[1-9][[:digit:]]?)|(00[1-9]))(\\.[[:digit:]]{0,2})?[[:space:]]*$", icd9) # not quite right, since it would validate 0.12

#' @title validate an icd9 mapping to comorbidities
#' @description just takes each item in each vector of the list of vectors and checks validity
#' @template mapping
#' @export
icd9ValidMappingShort <- function(icd9Mapping) all(unlist(lapply(icd9Mapping, FUN = icd9ValidShort), use.names=F))

#' @describeIn icd9ValidMappingShort
#' @export
icd9ValidMappingDecimal <- function(icd9Mapping) all(unlist(lapply(icd9Mapping, FUN = icd9ValidDecimal), use.names=F))

#' @describeIn icd9ValidDecimal
#' @export
icd9ValidMappingShort <- function(icd9Mapping) all(unlist(lapply(icd9Mapping, FUN = icd9ValidShort), use.names=F))

#' @describeIn icd9ValidDecimal
#' @export
getInvalidDecimalIcd9 <- function(icd9) icd9[!icd9ValidDecimal(icd9)]

#' @describeIn icd9ValidDecimal
#' @export
getInvalidShortICD9 <- function(icd9) icd9[!icd9ValidShort(icd9)]
