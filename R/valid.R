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

#' Check whether ICD-9 codes are syntactically valid
#'
#' This does not check whether the code coresponds to a real ICD-9-CM billing
#' code, or parent grouping. For that, see \code{\link{icd9IsReal}}.
#'
#' Factors are accepted, and since the validation is done with \code{grepl}
#' these are handled correctly.
#'
#' Currently, there is a limitation on NA values. Calling with \code{NA} (which
#' is a logical vector of length one by default) fails, because it is not a
#' string. This is rarely of significance in real life, since the NA will be
#' part of a character vector of codes, and will therefore be cast already to
#' \code{NA_character}
#'
#' \code{NA} values result in a return value of \code{FALSE}.
#' @section Three-digit validation: \code{isValidMajor} validates just the
#'   'major' three-digit part of an ICD-9 code. This can in fact be provided as
#'   a numeric, since there is no ambiguity. Numeric-only codes should be one to
#'   three digitis, V codes are followed by one or two digits, and E codes
#'   always by three digits between 800 and 999.
#' @details Leading zeroes in the decimal form are not ambiguous. Although
#'   integer ICD-9 codes could be intended by the user, there is a difference
#'   between 100, 100.0, 100.00. Therefore a warning is given if a numeric value
#'   is provided TODO: add default (when there is no class) which detected icd9
#'   vs 10 if possible. TODO: use "short_code" or "long" attribute if available to
#'   tighten validation, or guess if missing.
#' @section Class: S3 class of on object in R is just a vector. Attributes are
#'   lost with manipulation, with the exception of class: therefore, elements of
#'   the class vector are used to describe features of the data. If these are
#'   not present, the user may specify (e.g. decimal vs short_code type, ICD-9 vs
#'   ICD-10 WHO), but if they are, the correct functions are called without any
#'   guess work. There are overlapping namespaces for short_code vs decimal and ICD-9
#'   vs ICD-10 codes, so guessing is never going to be perfect.
#'
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template major
#' @param icd An ICD-9 or 10 code. If the class is set to \code{'icd9'},
#'   \code{'icd10'}, \code{'icd10cm'} etc then perform appropriate validation.
#' @seealso \url{http://www.stata.com/users/wgould/icd9/icd9.hlp}
#'   url{http://www.sascommunity.org/wiki/Validate_the_format_of_ICD-9_codes}
#' @family ICD9 validation
#' @return logical vector with \code{TRUE} or \code{FALSE} for each icd9 code
#'   provided according to its validity
#' @examples
#'   icd_validShort(c("", "1", "22", "333", "4444", "123.45", "V",
#'                      "V2", "V34", "V567", "E", "E1", "E70", "E"))
#'   icd_validMajor(c("", "1", "22", "333", "4444", "123.45", "V",
#'                      "V2", "V34", "V567", "E", "E1", "E70", "E"))
#' @export
icd_is_valid <- function(icd, ...) {
  if (inherits(icd, what = "icd_short_code"))
    NextMethod(short_code = TRUE)
  else if (inherits(icd, what = "icd_decimal_code"))
    NextMethod(short_code = FALSE)
  else
    UseMethod("icd_is_valid")
}

#' @describeIn icd_is_valid Test whether generic ICD-10 code is valid
#' @import checkmate stringr
#' @export
icd_is_valid.icd10 <- function(icd, short_code = icd_guess_short(icd)) {
  assertCharacter(icd)
  # SOMEDAY: check whether code has 'year' attribute. This is maybe more for testing 'realness'
  # start with a broad regex

  # TODO: test whether icd-10-cm or WHO, if class not otherwise specified.
  if (short_code)
    icd %>% str_trim %>% str_detect("^[[:space:]]*[[:alpha:]][[:digit:]][[:alnum:]]{1,4}[[:space:]]*$")
  else
    icd %>% str_trim %>% str_detect("^[[:space:]]*[[:alpha:]][[:digit:]][[:alnum:]]\\.[[:alnum:]]{0,4}[[:space:]]*$")
}

#' @describeIn icd_is_valid Test whether generic ICD-10 code is valid
#' @export
icd_is_valid.icd9 <- function(x, short_code) {
  checkmate::assert(
    checkmate::checkFactor(x),
    checkmate::checkCharacter(x),
    checkmate::checkClass(x, c("icd9")), # TODO: use icd9_classes
    checkmate::checkClass(x, c("icd9cm"))
  )
  checkmate::assertFlag(short_code)
  if (short_code)
    icd9_is_valid_short(x)
  else
    icd9_is_valid_decimal(x)
}

icd_is_valid.character <- function(x, short_code = icd_guess_short(x)) {
  ver <- icd_guess_version(x)
  switch(ver,
         "icd9" = icd_is_valid.icd9(x, short_code),
         "icd10" = icd_is_valid.icd10(x, short_code)
  )
}

icd9_is_valid_decimal <- function(x) {
  assert(checkFactor(x), checkCharacter(x), checkClass("icd9"), checkClass("icd9cm"))
  if (length(x) == 0) return(logical())

  icd9_is_valid_decimal_n(x) |
    icd9_is_valid_decimal_v(x) |
    icd9_is_valid_decimal_e(x)
}

icd9_is_valid_short <- function(x) {
  # if input doesn't satisfy these, then it is not just invalid, but deserves an error:
  checkmate::assert(
    checkmate::checkFactor(x),
    checkmate::checkCharacter(x),
    checkmate::checkClass(x, c("icd9")),
    checkmate::checkClass(x, c("icd9cm"))
  )
  if (length(x) == 0) return(logical())

  # as explained in details, a numeric short_code ID has different validity
  # requirements than a string because of leading zeroes.
  icd9_is_valid_short_n(x) |
    icd9_is_valid_short_v(x) |
    icd9_is_valid_short_e(x)
}

icd9_is_valid_short_v <- function(x) {
  x <- asCharacterNoWarn(x)
  grepl("^[[:space:]]*[Vv](([1-9][[:digit:]]?)|([[:digit:]][1-9]))[[:digit:]]{0,2}[[:space:]]*$", # nolint
        x)
}

icd9_is_valid_short_e <- function(icd){
  icd <- asCharacterNoWarn(icd)
  grepl("^[[:space:]]*[Ee][[:digit:]]{1,4}[[:space:]]*$", icd)
}

icd9_is_valid_short_n <- function(icd) {
  icd <- asCharacterNoWarn(icd)
  grepl("^[[:space:]]*[[:digit:]]{1,5}[[:space:]]*$", icd)
}

icd9_is_valid_decimal_v <- function(icd) {
  icd <- asCharacterNoWarn(icd)
  grepl("^[[:space:]]*[Vv](([1-9][[:digit:]]?)|([[:digit:]][1-9]))(\\.[[:digit:]]{0,2})?[[:space:]]*$", # nolint
        icd)
}

icd9_is_valid_decimal_e <- function(x) {
  #need Perl regex for lookbehind. may even be quicker, according to the docs.
  #grepl("^E(?!0+($|\\.))[[:digit:]][[:digit:]]{0,2}(\\.[[:digit:]]?)?$",
  #trim(icd), perl = TRUE)
  assert(checkFactor(x), checkCharacter(x))
  x <- asCharacterNoWarn(x)
  grepl("^[[:space:]]*[Ee][[:digit:]]{1,3}(\\.[[:digit:]]?)?[[:space:]]*$", x)
}

icd9_is_valid_decimal_n <- function(x) {
  assert(checkFactor(x), checkCharacter(x))
  x <- asCharacterNoWarn(x)
  grepl("^[[:space:]]*((0{1,3})|([1-9][[:digit:]]{0,2})|(0[1-9][[:digit:]]?)|(00[1-9]))(\\.[[:digit:]]{0,2})?[[:space:]]*$", # nolint
        x)
}

# do i need to export this?
icd_is_valid_major <- function(x)
  UseMethod("icd_is_valid_major")

icd_is_valid_major.icd9 <- function(major)
  # let grepl do what it can with integers, factors, etc.
  grepl(
    pattern = "^[[:space:]]*([[:digit:]]{1,3}[[:space:]]*$)|([Vv][[:digit:]]{1,2}[[:space:]]*$)|([Ee][[:digit:]]{1,3}[[:space:]]*$)", # nolint
    x = major
  )

icd9_is_valid_major_n <- function(major)
  grepl(
    pattern = "^[[:space:]]*[[:digit:]]{1,3}[[:space:]]*$",
    x = major
  )

icd9_is_valid_major_v <- function(major)
  grepl(
    pattern = "^[[:space:]]*[Vv][[:digit:]]{1,2}[[:space:]]*$",
    x = major
  )

icd9_is_valid_major_e <- function(major)
  grepl(
    pattern = "^[[:space:]]*[Ee][[:digit:]]{1,3}[[:space:]]*$",
    x = major
  )

#' validate an icd9 mapping to comorbidities
#'
#' @description takes each item in each vector of the list of vectors and checks
#'   validity, or returns those items which are valid for each comorbidity.
#' @template short_code
#' @template mapping
#' @family ICD9 validation
#' @export
icd_is_valid_map <- function(map, short_code) {
  checkmate::assertList(map, types = "character", any.missing = FALSE,
                        min.len = 1, unique = TRUE, names = "named")
  checkmate::assertFlag(short_code)
  # TOOD: warn/return the invalid labels
  all(unlist(
    lapply(map, FUN = function(x) icd_is_valid(x, short_code)),
    use.names = FALSE
  ))
}

#' invalid subset of decimal or short_code ICD-9 codes
#'
#' Given vector of short_code or decimal ICD-9 codes, return (in the
#'   same format) those codes which are valid or invalid. Useful for generating
#'   error messages with the faulty codes if validation fails.
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template short_code
#' @keywords manip
#' @family ICD-9 validation
#' @export
icd_get_valid <- function(icd, short_code = icd_guess_short(icd))
  UseMethod("icd_get_valid")

#' @describeIn icd_get_valid get valid ICD codes from character vector, guessing ICD version
icd_get_valid.character <- function(icd, short_code = icd_guess_short(icd)) {
  icd_ver <- icd_guess_version.character(icd, short_code)
  class(icd) <- icd_ver
  # now, this is risky: dispatch again
  icd_get_valid(icd, short_code)
}

#' @describeIn icd_get_valid Get valid ICD-9 codes
#' @export
icd_get_valid.icd9 <- function(icd, short_code = icd_guess_short(icd)) {
  icd[icd_is_valid.icd9(icd, short_code = short_code)]
}

#' @describeIn icd_get_valid Get valid ICD-10 codes
#' @export
icd_get_valid.icd10 <- function(icd, short_code = icd_guess_short(icd)) {
  icd[icd_is_valid.icd10(icd, short_code = short_code)]
}

#' @describeIn icd_get_valid Get valid ICD-10-CM codes
#' @export
icd_get_valid.icd10cm <- function(icd, short_code = icd_guess_short(icd)) {
  icd[icd_is_valid.icd10cm(icd, short_code = short_code)]
}

#' @title Get invalid ICD codes
#' @description Returns subset of codes which are not in valid short_code or decimal format.
#' @export
icd_get_invalid <- function(...) 
  UseMethod("icd_get_invalid")

#' @describeIn icd_get_invalid Get invalid ICD-9 codes from vector of codes
#' @param x vector of ICD codes, or list of vectors of ICD codes forming a comorbidity map
#' @export
icd_get_invalid.icd9 <- function(x, short_code = icd_guess_short.icd9(icd9)) {
  x[!icd_is_valid.icd9(x, short_code = short_code)]
}

#' @describeIn icd_get_invalid Get invalid elements of a comorbidity map
#' @export
icd_get_invalid.map <- function(map, short_code = icd_guess_short(map)) {
 # todo: may need to switch on ICD code type
  x <- lapply(map, FUN = icd_get_invalid, short_code = short_code)
  x[lapply(x, length) > 0]
}

#' Returns major component of code (before decimal)
#' @keywords internal
icd_get_major <- function(x) {
  UseMethod("icd_get_major")
}

#' check whether a code is major
#' @description move to C++ or own R file:
#' @param icd character vector of ICD codes.
#' @keywords internal
icd_is_major <- function(icd) {
  UseMethod("icd_is_major")
}

#' @describeIn icd_is_major check whether a code is an ICD-10 major
#' @keywords internal
icd_is_major.icd10 <- function(icd) {
  assertCharacter(icd)
  # if not know whether ICD-10-CM, then use broader definition
  icd_is_major.icd10cm(icd)
}

#' @describeIn icd_is_major check whether a code is an ICD-10-CM major
#' @keywords internal
icd_is_major.icd10cm <- function(icd) {
  assertCharacter(icd)
  stringr::str_detect(icd, "^[[:space:]]*[[:alpha:]][[:digit:]][[:alnum:]][[:space:]]*$")
}

#' @describeIn icd_is_major check whether a code is an ICD-10 WHO major
#' @keywords internal
icd_is_major.icd10who <- function(icd) {
  assertCharacter(icd)
  stringr::str_detect(icd, "^[[:space:]]*[[:alpha:]][[:digit:]][[:digit:]][[:space:]]*$")
}

#' @describeIn icd_is_major check whether a code is an ICD-9 major
#' @keywords internal
icd_is_major.icd9 <- function(x) {
  x <- trim(x)
  nchar(x) - icd9_is_e(x) < 4
}

#' do ICD-9 codes belong to numeric, V or E classes?
#'
#' For each code, return \code{TRUE} if numric or \code{FALSE} if a
#'   V or E code.
#' @template icd9-any
#' @return logical vector
#' @export
icd9_is_n <- function(icd9) {
  icd9IsA(asCharacterNoWarn(icd9), "VEve", TRUE)
}

#' @describeIn icd9_is_n are the given codes V type?
#' @export
icd9_is_v <- function(icd9) {
  icd9IsA(asCharacterNoWarn(icd9), "Vv")
}

#' @describeIn icd9_is_n are the given codes E type?
#' @export
icd9_is_e <- function(icd9) {
  icd9IsA(asCharacterNoWarn(icd9), "Ee")
}

warnNumericCode <- function()
  warning("input data is in numeric format. This can easily lead to errors in short_code or decimal codes, e.g. short_code code 1000: is it 10.00 or 100.0; or decimal codes, e.g. 10.1 was supposed to be 10.10", call. = FALSE) # nolint

#' @describeIn icd_is_valid Check whether ICD-10 WHO codes are valid
#' @details From WHO ICD-10 manual: "The basic ICD is a single coded list of
#'   three-character categories, each of which can be further divided into up to
#'   10 four-character subcategories. In place of the purely numeric coding
#'   system of previous revisions, the 10th revision uses an alphanumeric code
#'   with a letter in the first position and a number in the second, third and
#'   fourth positions. The fourth character follows a decimal point. Possible
#'   code numbers therefore range from A00.0 to Z99.9. The letter U is not used
#'   (see Section 2.4.7)."
#'
#'   "Although not mandatory for reporting at the international level, most of
#'   the three-character categories are subdivided by means of a fourth, numeric
#'   character after a decimal point, allowing up to 10 subcategories. Where a
#'   three-character category is not subdivided, it is recommended that the
#'   letter ‘X’ be used to fill the fourth position, so that the codes are of a
#'   standard length for data-processing."
#'
#'   officially, WHO standard goes to 3+1 digits, but there are officially
#'   sanctioned extensions which use additional digits, e.g. neurology,
#'   pediatrics, and of course ICD-10-CM.
#' @export
icd_is_valid.icd10who <- function(icd, short_code = icd_guess_short.icd10(icd)) {
  assertCharacter(icd)
  assertFlag(short_code)
  # SOMEDAY: check whether code has 'year' attribute. This is maybe more for testing 'realness'
  # start with a broad regex

  icd %>%
    str_trim %>%
    str_detect("^[[:alpha:]][[:digit:]][[:digit:]]\\.?(X|[[:digit:]]*)$")

}
