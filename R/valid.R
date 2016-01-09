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
#' code, or parent grouping. For that, see \code{\link{icd_is_defined}}.
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
#' @section Three-digit validation: \code{icd9_is_valid_major} validates just
#'   the 'major' three-digit part of an ICD-9 code. This can in fact be provided
#'   as a numeric, since there is no ambiguity. Numeric-only codes should be one
#'   to three digitis, V codes are followed by one or two digits, and E codes
#'   always by three digits between 800 and 999.
#' @details Leading zeroes in the decimal form are not ambiguous. Although
#'   integer ICD-9 codes could be intended by the user, there is a difference
#'   between 100, 100.0, 100.00. Therefore a warning is given if a numeric value
#'   is provided TODO: add default (when there is no class) which detected icd9
#'   vs 10 if possible. TODO: use "short_code" or "long" attribute if available
#'   to tighten validation, or guess if missing.
#' @section Class: S3 class of on object in R is just a vector. Attributes are
#'   lost with manipulation, with the exception of class: therefore, elements of
#'   the class vector are used to describe features of the data. If these are
#'   not present, the user may specify (e.g. decimal vs short_code type, ICD-9
#'   vs ICD-10 WHO), but if they are, the correct functions are called without
#'   any guess work. There are overlapping namespaces for short_code vs decimal
#'   and ICD-9 vs ICD-10 codes, so guessing is never going to be perfect.
#'
#' @param x An ICD-9 or 10 code. If the class is set to \code{'icd9'},
#'   \code{'icd10'}, \code{'icd10cm'} etc then perform appropriate validation.
#' @template major
#' @template dotdotdot
#' @seealso \url{http://www.stata.com/users/wgould/icd9/icd9.hlp}
#'   url{http://www.sascommunity.org/wiki/Validate_the_format_of_ICD-9_codes}
#' @family ICD9 validation
#' @return logical vector with \code{TRUE} or \code{FALSE} for each icd9 code
#'   provided according to its validity
#' @examples
#'   icd_is_valid(c("", "1", "22", "333", "4444", "123.45", "V",
#'                      "V2", "V34", "V567", "E", "E1", "E70", "E"))
#'   # internal function:
#'   \dontrun{
#'   icd9:::icd_is_valid_major(c("", "1", "22", "333", "4444", "123.45", "V",
#'                      "V2", "V34", "V567", "E", "E1", "E70", "E"))
#'   }
#' @export
icd_is_valid <- function(x, ...) {
  UseMethod("icd_is_valid")
}

#' @describeIn icd_is_valid Test whether generic ICD-10 code is valid
#' @export
icd_is_valid.icd10 <- function(x, short_code = icd_guess_short(x), ...) {
  assertCharacter(x)
  # SOMEDAY: check whether code has 'year' attribute. This is maybe more for testing 'realness'
  # start with a broad regex

  # TODO: test whether icd-10-cm or WHO, if class not otherwise specified.
  if (short_code)
    x %>% str_trim() %>% str_detect("^[[:space:]]*[[:alpha:]][[:digit:]][[:alnum:]]{1,4}[[:space:]]*$")
  else
    x %>% str_trim() %>% str_detect("^[[:space:]]*[[:alpha:]][[:digit:]][[:alnum:]]\\.[[:alnum:]]{0,4}[[:space:]]*$")
}

#' @describeIn icd_is_valid Test whether generic ICD-10 code is valid
#' @export
icd_is_valid.icd9 <- function(x, short_code = icd_guess_short.icd9(x), ...) {
  assert(
    checkFactor(x),
    checkCharacter(x),
    checkClass(x, c("icd9")), # TODO: use icd9_classes
    checkClass(x, c("icd9cm"))
  )
  assertFlag(short_code)
  if (short_code)
    icd9_is_valid_short(x)
  else
    icd9_is_valid_decimal(x)
}

#' @describeIn icd_is_valid Test whether a character vector of ICD vodes is
#'   valid, guessing both type and version of the ICD codes
#' @export
icd_is_valid.character <- function(x, short_code = icd_guess_short(x), ...) {
  ver <- icd_guess_version(x)
  switch(ver,
         "icd9" = icd_is_valid.icd9(x, short_code),
         "icd10" = icd_is_valid.icd10(x, short_code)
  )
}

icd9_is_valid_decimal <- function(x) {
  assert(checkFactor(x), checkCharacter(x), checkClass("icd9"), checkClass("icd9cm"))
  if (length(x) == 0)
    return(logical())

  icd9_is_valid_decimal_n(x) |
    icd9_is_valid_decimal_v(x) |
    icd9_is_valid_decimal_e(x)
}

icd9_is_valid_short <- function(x) {
  # if input doesn't satisfy these, then it is not just invalid, but deserves an error:
  assert(
    checkFactor(x),
    checkCharacter(x),
    checkClass(x, c("icd9")),
    checkClass(x, c("icd9cm"))
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

icd9_is_valid_short_e <- function(x){
  x <- asCharacterNoWarn(x)
  grepl("^[[:space:]]*[Ee][[:digit:]]{1,4}[[:space:]]*$", x)
}

icd9_is_valid_short_n <- function(x) {
  x <- asCharacterNoWarn(x)
  grepl("^[[:space:]]*[[:digit:]]{1,5}[[:space:]]*$", x)
}

icd9_is_valid_decimal_v <- function(x) {
  x <- asCharacterNoWarn(x)
  grepl("^[[:space:]]*[Vv](([1-9][[:digit:]]?)|([[:digit:]][1-9]))(\\.[[:digit:]]{0,2})?[[:space:]]*$", # nolint
        x)
}

icd9_is_valid_decimal_e <- function(x) {
  #need Perl regex for lookbehind. may even be quicker, according to the docs.
  #grepl("^E(?!0+($|\\.))[[:digit:]][[:digit:]]{0,2}(\\.[[:digit:]]?)?$",
  #trim(x), perl = TRUE)
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

#' @title Test whether an ICD code is major
#' @description codes without real or implied decimal place return TRUE
#' @param x vector of ICD codes
#' @return logical vector of same length as input, with TRUE when a code is a
#'   major (not necessarily a real one)
#' @keywords internal
icd_is_valid_major <- function(x) {
  UseMethod("icd_is_valid_major")
}

#' @describeIn icd_is_valid_major Test whether an ICD code is of major type,
#'   which at present assumes ICD-9 format
#' @export
#' @keywords internal
icd_is_valid_major.default <- function(x) {
  icd_is_valid_major.icd9(asCharacterNoWarn(x))
}

#' @describeIn icd_is_valid_major Test whether an ICD-9 code is of major type.
#' @export
#' @keywords internal
icd_is_valid_major.icd9 <- function(x)
  # let grepl do what it can with integers, factors, etc.
  grepl(
    pattern = "^[[:space:]]*([[:digit:]]{1,3}[[:space:]]*$)|([Vv][[:digit:]]{1,2}[[:space:]]*$)|([Ee][[:digit:]]{1,3}[[:space:]]*$)", # nolint
    x = x
  )

#' @rdname icd_is_valid_major
#' @keywords internal
icd9_is_valid_major_n <- function(x)
  grepl(
    pattern = "^[[:space:]]*[[:digit:]]{1,3}[[:space:]]*$",
    x = x
  )

#' @rdname icd_is_valid_major
#' @keywords internal
icd9_is_valid_major_v <- function(x)
  grepl(
    pattern = "^[[:space:]]*[Vv][[:digit:]]{1,2}[[:space:]]*$",
    x = x
  )

#' @rdname icd_is_valid_major
#' @keywords internal
icd9_is_valid_major_e <- function(x)
  grepl(
    pattern = "^[[:space:]]*[Ee][[:digit:]]{1,3}[[:space:]]*$",
    x = x
  )

#' @describeIn icd_is_valid Validate an icd9 mapping to comorbidities
#' @export
#' @keywords internal
icd_is_valid.icd_comorbidity_map <- function(x, short_code, ...) {
  assertList(x, types = "character", any.missing = FALSE,
                        min.len = 1, unique = TRUE, names = "named")
  assertFlag(short_code)
  # TOOD: warn/return the invalid labels
  all(unlist(
    lapply(x, FUN = function(y) icd_is_valid(y, short_code)),
    use.names = FALSE
  ))
}

#' invalid subset of decimal or short_code ICD-9 codes
#'
#' Given vector of short_code or decimal ICD-9 codes, return (in the
#'   same format) those codes which are valid or invalid. Useful for generating
#'   error messages with the faulty codes if validation fails.
#' @param x input vector of ICD codes
#' @template short_code
#' @keywords manip
#' @family ICD-9 validation
#' @export
icd_get_valid <- function(x, short_code = icd_guess_short(x))
  UseMethod("icd_get_valid")

#' @describeIn icd_get_valid get valid ICD codes from character vector, guessing ICD version
icd_get_valid.character <- function(x, short_code = icd_guess_short(x)) {
  icd_ver <- icd_guess_version.character(x, short_code)
  class(x) <- icd_ver
  # now, this is risky: dispatch again
  icd_get_valid(x, short_code)
}

#' @describeIn icd_get_valid Get valid ICD-9 codes
#' @export
icd_get_valid.icd9 <- function(x, short_code = icd_guess_short(x)) {
  x[icd_is_valid.icd9(x, short_code = short_code)]
}

#' @describeIn icd_get_valid Get valid ICD-10 codes
#' @export
icd_get_valid.icd10 <- function(x, short_code = icd_guess_short(x)) {
  x[icd_is_valid.icd10(x, short_code = short_code)]
}

#' @describeIn icd_get_valid Get valid ICD-10-CM codes
#' @export
icd_get_valid.icd10cm <- function(x, short_code = icd_guess_short(x)) {
  # TODO: make ICD-10-CM specific
  x[icd_is_valid.icd10(x, short_code = short_code)]
}

#' @title Get invalid ICD codes
#' @description Returns subset of codes which are not in valid short_code or decimal format.
#' @export
icd_get_invalid <- function(...) {
  UseMethod("icd_get_invalid")
}

#' @describeIn icd_get_invalid Default method when ICD version or short versus
#'   decimal not known.
#' @export
#' @keywords internal
icd_get_invalid.default <- function(x, short_code = NULL, ...) {
  # both <- icd_guess_both(x, short_code = short_code)
  x %>% icd_guess_short_update %>% icd_guess_version_update %>% icd_get_invalid
}

#' @describeIn icd_get_invalid Get invalid ICD-9 codes from vector of codes
#' @param x vector of ICD codes, or list of vectors of ICD codes forming a
#'   comorbidity map
#' @export
icd_get_invalid.icd9 <- function(x, short_code = icd_guess_short.icd9(x), ...) {
  x[!icd_is_valid.icd9(x, short_code = short_code)]
}

#' @describeIn icd_get_invalid Get invalid elements of a comorbidity map
#' @export
icd_get_invalid.icd_comorbidity_map <- function(x, short_code = icd_guess_short(x), ...) {
  # todo: may need to switch on ICD code type
  x <- lapply(x, FUN = icd_get_invalid, short_code = short_code)
  x[lapply(x, length) > 0]
}

#' Returns major component of code (before decimal)
#' @keywords internal
icd_get_major <- function(x) {
  UseMethod("icd_get_major")
}

#' initial implementation via stringr. If speed needed, then can probably reuse
#' C++ ICD-9 version: i'm just grabbing the first three characters, after all,
#' and this is much easier in ICD-10 then ICD-9
#' @export
#' @keywords internal
icd_get_major.icd10 <- function(x) {
  x %>% str_trim %>% str_sub(1, 3)
}

#' Check whether a code is major
#' @param icd character vector of ICD codes.
#' @keywords internal
icd_is_major <- function(x) {
  UseMethod("icd_is_major")
}

#' @describeIn icd_is_major check whether a code is an ICD-10 major
#' @keywords internal
icd_is_major.icd10 <- function(x) {
  assertCharacter(x)
  # if not know whether ICD-10-CM, then use broader definition
  icd_is_major.icd10cm(x)
}

#' @describeIn icd_is_major check whether a code is an ICD-10-CM major.
#'   Currently uses \code{stringr} which uses \code{stringi} which should be
#'   quite fast, but does suffer from handling unicode, locales, etc.
#' @keywords internal
icd_is_major.icd10cm <- function(x) {
  assertCharacter(x)
  str_detect(x, "^[[:space:]]*[[:alpha:]][[:digit:]][[:alnum:]][[:space:]]*$")
}

#' @describeIn icd_is_major check whether a code is an ICD-10 WHO major
#' @keywords internal
icd_is_major.icd10who <- function(x) {
  assertCharacter(x)
  str_detect(x, "^[[:space:]]*[[:alpha:]][[:digit:]][[:digit:]][[:space:]]*$")
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
#' @param x vector of strings or factor to test
#' @return logical vector
#' @export
icd9_is_n <- function(x) {
  # icd9IsA(asCharacterNoWarn(x), "VEve", TRUE)
  icd9_is_n_cpp(asCharacterNoWarn(x))
}

#' @describeIn icd9_is_n are the given codes V type?
#' @export
icd9_is_v <- function(x) {
  icd9_is_v_cpp(asCharacterNoWarn(x))
  #icd9IsA(asCharacterNoWarn(x), "Vv", FALSE)
}

#' @describeIn icd9_is_n are the given codes E type?
#' @export
icd9_is_e <- function(x) {
  icd9_is_e_cpp(asCharacterNoWarn(x))
  #icd9IsA(asCharacterNoWarn(x), "Ee", FALSE)
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
#'   letter 'X' be used to fill the fourth position, so that the codes are of a
#'   standard length for data-processing."
#'
#'   Officially, WHO standard goes to 3+1 digits, but there are officially
#'   sanctioned extensions which use additional digits, e.g. neurology,
#'   pediatrics, and of course ICD-10-CM.
#' @export
icd_is_valid.icd10who <- function(x, short_code = icd_guess_short.icd10(x), ...) {
  assertCharacter(x)
  assertFlag(short_code)
  # SOMEDAY: check whether code has 'year' attribute. This is maybe more for
  # testing whether a code is defined for a particular set of ICD codes, e.g. an
  # annual revision of ICD-10-CM

  #  start with a broad regex

  str_trim(x) %>%
    str_detect("^[[:alpha:]][[:digit:]][[:digit:]]\\.?(X|[[:digit:]]*)$")

}
