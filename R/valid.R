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

#' @title check whether ICD-9 codes are syntactically valid
#' @description This does not check whether the code coresponds to a real
#'   ICD-9-CM billing code, or parent grouping. For that, see
#'   \code{\link{icd9IsReal}}.
#'
#'   Factors are accepted, and since the validation is done with \code{grepl}
#'   these are handled correctly.
#'
#'   Currently, there is a limitation on NA values. Calling with \code{NA}
#'   (which is a logical vector of length one by default) fails, because it is
#'   not a string. This is rarely of significance in real life, since the NA
#'   will be part of a character vector of codes, and will therefore be cast
#'   already to \code{NA_character}
#'
#'   \code{NA} values result in a return value of \code{FALSE}.
#' @section Three-digit validation: \code{isValidMajor} validates just the
#'   'major' three-digit part of an ICD-9 code. This can in fact be provided as
#'   a numeric, since there is no ambiguity. Numeric-only codes should be one to
#'   three digitis, V codes are followed by one or two digits, and E codes
#'   always by three digits between 800 and 999.
#' @template major
#' @details Leading zeroes in the decimal form are not ambiguous. Although
#'   integer ICD-9 codes could be intended by the user, there is a difference
#'   between 100, 100.0, 100.00. Therefore a warning is given if a numeric value
#'   is provided
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template isShort
#' @seealso \code{\link{icd9IsValidDecimal}}, \code{\link{icd9IsValidShort}},
#'   \url{http://www.stata.com/users/wgould/icd9/icd9.hlp}
#'   url{http://www.sascommunity.org/wiki/Validate_the_format_of_ICD-9_codes}
#' @family ICD9 validation
#' @return logical vector with \code{TRUE} or \code{FALSE} for each icd9 code
#'   provided according to its validity
#' @examples
#'   icd9IsValidShort(c("", "1", "22", "333", "4444", "123.45", "V",
#'                      "V2", "V34", "V567", "E", "E1", "E70", "E"))
#'   icd9IsValidMajor(c("", "1", "22", "333", "4444", "123.45", "V",
#'                      "V2", "V34", "V567", "E", "E1", "E70", "E"))
#' @export
icd9IsValid <- function(icd9, isShort) {
  assertFlag(isShort)
  if (isShort) icd9IsValidShort(icd9) else icd9IsValidDecimal(icd9)
}

#' @rdname icd9IsValid
#' @export
icd9Valid <- function(icd9, isShort) {
  warning("icd9Valid is deprecated, please use icd9IsValid")
  icd9IsValid(icd9, isShort)
}

#' @rdname icd9IsValid
#' @export
icd9IsValidDecimal <- function(icd9Decimal) {
  assertFactorOrCharacter(icd9Decimal)
  if (length(icd9Decimal) == 0) return(logical())

  icd9IsValidDecimalN(icd9Decimal) |
    icd9IsValidDecimalV(icd9Decimal) |
    icd9IsValidDecimalE(icd9Decimal)
}

#' @rdname icd9IsValid
#' @export
icd9ValidDecimal <- function(icd9) {
  warning("icd9ValidDecimal is deprecated, please use icd9IsValidDecimal")
  icd9IsValidDecimal(icd9)
}

#' @rdname icd9IsValid
#' @export
icd9IsValidShort <- function(icd9Short) {

  if (length(icd9Short) == 0) return(logical())

  assertFactorOrCharacter(icd9Short)

  # as explained in details, a numeric short ID has different validity
  # requirements than a string because of leading zeroes.
  icd9IsValidShortN(icd9Short) |
    icd9IsValidShortV(icd9Short) |
    icd9IsValidShortE(icd9Short)
}

#' @rdname icd9IsValid
#' @export
icd9ValidShort <- function(icd9) {
  warning("icd9ValidShort is deprecated, please use icd9IsValidShort")
  icd9IsValidShort(icd9)
}

#' @rdname icd9IsValid
#' @export
icd9IsValidShortV <- function(icd9Short) {
  assertFactorOrCharacter(icd9Short)
  icd9Short <- asCharacterNoWarn(icd9Short)
  grepl("^[[:space:]]*[Vv](([1-9][[:digit:]]?)|([[:digit:]][1-9]))[[:digit:]]{0,2}[[:space:]]*$", # nolint
        icd9Short)
}

#' @rdname icd9IsValid
#' @export
icd9IsValidShortE <- function(icd9Short){
  assertFactorOrCharacter(icd9Short)
  icd9Short <- asCharacterNoWarn(icd9Short)
  grepl("^[[:space:]]*[Ee][[:digit:]]{1,4}[[:space:]]*$", icd9Short)
}

#' @rdname icd9IsValid
#' @export
icd9IsValidShortN <- function(icd9Short) {
  assertFactorOrCharacter(icd9Short)
  icd9Short <- asCharacterNoWarn(icd9Short)
  grepl("^[[:space:]]*[[:digit:]]{1,5}[[:space:]]*$", icd9Short)
}

icd9IsValidDecimalV <- function(icd9Decimal) {
  assertFactorOrCharacter(icd9Decimal)
  icd9Decimal <- asCharacterNoWarn(icd9Decimal)
  grepl("^[[:space:]]*[Vv](([1-9][[:digit:]]?)|([[:digit:]][1-9]))(\\.[[:digit:]]{0,2})?[[:space:]]*$", # nolint
        icd9Decimal)
}

icd9IsValidDecimalE <- function(icd9Decimal) {
  #need Perl regex for lookbehind. may even be quicker, according to the docs.
  #grepl("^E(?!0+($|\\.))[[:digit:]][[:digit:]]{0,2}(\\.[[:digit:]]?)?$",
  #trim(icd9Decimal), perl = TRUE)
  assertFactorOrCharacter(icd9Decimal)
  icd9Decimal <- asCharacterNoWarn(icd9Decimal)
  grepl("^[[:space:]]*[Ee][[:digit:]]{1,3}(\\.[[:digit:]]?)?[[:space:]]*$",
        icd9Decimal)
}

icd9IsValidDecimalN <- function(icd9Decimal) {
  assertFactorOrCharacter(icd9Decimal)
  icd9Decimal <- asCharacterNoWarn(icd9Decimal)
  grepl("^[[:space:]]*((0{1,3})|([1-9][[:digit:]]{0,2})|(0[1-9][[:digit:]]?)|(00[1-9]))(\\.[[:digit:]]{0,2})?[[:space:]]*$", # nolint
        icd9Decimal)
}

#' @rdname icd9IsValid
#' @export
icd9IsValidMajor <- function(major)
  # let grepl do what it can with integers, factors, etc.
  grepl(
    pattern = "^[[:space:]]*([[:digit:]]{1,3}[[:space:]]*$)|([Vv][[:digit:]]{1,2}[[:space:]]*$)|([Ee][[:digit:]]{1,3}[[:space:]]*$)", # nolint
    x = major
  )

icd9IsValidMajorN <- function(major)
  grepl(
    pattern = "^[[:space:]]*[[:digit:]]{1,3}[[:space:]]*$",
    x = major
  )

icd9IsValidMajorV <- function(major)
  grepl(
    pattern = "^[[:space:]]*[Vv][[:digit:]]{1,2}[[:space:]]*$",
    x = major
  )

icd9IsValidMajorE <- function(major)
  grepl(
    pattern = "^[[:space:]]*[Ee][[:digit:]]{1,3}[[:space:]]*$",
    x = major
  )

#' @title validate an icd9 mapping to comorbidities
#' @description takes each item in each vector of the list of vectors and checks
#'   validity, or returns those items which are valid for each comorbidity.
#' @template mapping
#' @template isShort
#' @family ICD9 validation
#' @export
icd9IsValidMapping <- function(icd9Mapping, isShort) {
  assertList(icd9Mapping, types = "character", any.missing = FALSE,
             min.len = 1, unique = TRUE, names = "named")
  checkFlag(isShort)
  # TOOD: warn/return the invalid labels
  all(unlist(
    lapply(icd9Mapping, FUN = function(icd9Map) icd9IsValid(icd9Map, isShort)),
    use.names = FALSE
  ))
}

#' @rdname icd9IsValidMapping
#' @export
icd9IsValidMappingShort <- function(icd9Mapping)
  icd9IsValidMapping(icd9Mapping, TRUE)

#' @rdname icd9IsValidMapping
#' @export
icd9IsValidMappingDecimal <- function(icd9Mapping)
  icd9IsValidMapping(icd9Mapping, FALSE)

#' @rdname icd9IsValidMapping
#' @export
icd9GetInvalidMappingShort <- function(icd9Mapping) {
  x <- lapply(icd9Mapping, FUN = icd9GetInvalidShort)
  x[lapply(x, length) > 0]
}

7#' @rdname icd9IsValidMapping
#' @export
icd9GetInvalidMappingDecimal <- function(icd9Mapping) {
  x <- lapply(icd9Mapping, FUN = icd9GetInvalidDecimal)
  x[lapply(x, length) > 0]
}

#' @name icd9GetValid
#' @title invalid subset of decimal or short ICD-9 codes
#' @description Given vector of short or decimal ICD-9 codes, return (in the
#'   same format) those codes which are valid or invalid. Useful for generating
#'   error messages with the faulty codes if validation fails.
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template isShort
#' @keywords manip
#' @family ICD-9 validation
#' @export
icd9GetValid <- function(icd9, isShort = icd9GuessIsShort(icd9))
  icd9[icd9IsValid(icd9, isShort = isShort)]

#' @describeIn icd9GetValid Returns subset of codes which are in valid decimal format, e.g. "100" or "V01.10"
#' @export
icd9GetValidDecimal <- function(icd9Decimal)
  icd9Decimal[icd9IsValidDecimal(icd9Decimal)]

#' @describeIn icd9GetValid Returns subset of codes which are in valid short format, e.g. "E800" or "41001"
#' @export
icd9GetValidShort <- function(icd9Short)
  icd9Short[icd9IsValidShort(icd9Short)]

#' @describeIn icd9GetValid Returns subset of codes which are not in valid short or decimal format.
#' @export
icd9GetInvalid <- function(icd9, isShort = icd9GuessIsShort(icd9))
  icd9[!icd9IsValid(icd9, isShort = isShort)]

#' @describeIn icd9GetValid Returns subset of codes which are not in valid decimal format.
#' @export
icd9GetInvalidDecimal <- function(icd9Decimal)
  icd9Decimal[!icd9IsValidDecimal(icd9Decimal)]

#' @describeIn icd9GetValid Returns subset of codes which are not in valid short format.
#' @export
icd9GetInvalidShort <- function(icd9Short)
  icd9Short[!icd9IsValidShort(icd9Short)]

#' @keywords internal
icd9IsMajor <- function(icd9) {
  icd9 <- trim(icd9)
  nchar(icd9) - icd9IsE(icd9) < 4
}

#' @title do codes belong to numeric, V or E classes?
#' @description For each code, return \code{TRUE} if numric or \code{FALSE} if a
#'   V or E code.
#' @template icd9-any
#' @return logical vector
#' @export
icd9IsN <- function(icd9)
  icd9IsA(asCharacterNoWarn(icd9), "VEve", TRUE)

#' @describeIn icd9IsN are the given codes V type?
#' @export
icd9IsV <- function(icd9)
  icd9IsA(asCharacterNoWarn(icd9), "Vv")


#' @describeIn icd9IsN are the given codes E type?
#' @export
icd9IsE <- function(icd9)
  icd9IsA(asCharacterNoWarn(icd9), "Ee")

warnNumericCode <- function()
  warning("input data is in numeric format. This can easily lead to errors in short or decimal codes, e.g. short code 1000: is it 10.00 or 100.0; or decimal codes, e.g. 10.1 was supposed to be 10.10", call. = FALSE) # nolint
