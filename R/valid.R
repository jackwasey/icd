# Copyright (C) 2014 - 2016  Jack O. Wasey
#
# This file is part of icd.
#
# icd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd. If not, see <http:#www.gnu.org/licenses/>.

#' limit a regex to just what is given
#'
#' update regex to limit by start and end, with or without whitespace
#' @param x single string containing a regex
#' @template whitespace_ok
#' @keywords internal
re_just <- function(x, whitespace_ok = FALSE) {
  assert_string(x)
  assert_flag(whitespace_ok)
  if (whitespace_ok)
    paste0("^[[:space:]]*", x, "[[:space:]]*$")
  else
    paste0("^", x, "$")
}

#' @describeIn re_just allow whitespace
#' @keywords internal
re_just_ws <- function(x) {
  re_just(x, whitespace_ok = TRUE)
}

#' @describeIn re_just contain any \code{|} options within a regex, e.g. ICD-9 codes without
#'   \code{^} or \code{$}.
#' @keywords internal
re_wrap_or <- function(x) {
  paste0("(?:", x, ")")
}

re_icd9_major_n <- "[[:digit:]]{1,3}"
# "^[[:space:]]*((0{1,3})|([1-9][[:digit:]]{0,2})|(0[1-9][[:digit:]]?)|(00[1-9]))(\\.[[:digit:]]{0,2})?[[:space:]]*$", # nolint
re_icd9_major_n_strict <- "[[:digit:]]{3}"
# TODO: think about: "^[[:space:]]*[Vv](([1-9][[:digit:]]?)|([[:digit:]][1-9]))[[:digit:]]{0,2}[[:space:]]*$", # nolint
# "^[[:space:]]*[Vv](([1-9][[:digit:]]?)|([[:digit:]][1-9]))(\\.[[:digit:]]{0,2})?[[:space:]]*$"
re_icd9_major_v <- "[Vv](?:0[1-9]|[1-9][[:digit:]]?)"
re_icd9_major_v_strict <- "V(?:0[1-9]|[1-9][[:digit:]])"
re_icd9_major_e <- "[Ee][[:digit:]]{1,3}"
re_icd9_major_e_strict <- "E[[:digit:]]{3}"
re_icd9_major <- paste0(c(re_icd9_major_n %>% re_just_ws,
                          re_icd9_major_v %>% re_just_ws,
                          re_icd9_major_e %>% re_just_ws),
                        collapse = "|")
re_icd9_major_bare <- paste0(c(re_icd9_major_n,
                          re_icd9_major_v,
                          re_icd9_major_e),
                        collapse = "|") %>% re_wrap_or
re_icd9_major_strict <- paste0(c(re_icd9_major_n_strict %>% re_just_ws,
                                 re_icd9_major_v_strict %>% re_just_ws,
                                 re_icd9_major_e_strict %>% re_just_ws),
                               collapse = "|")
re_icd9_major_strict_bare <- paste0(c(re_icd9_major_n_strict,
                                 re_icd9_major_v_strict,
                                 re_icd9_major_e_strict),
                               collapse = "|") %>% re_wrap_or
re_icd9_minor_nv <- "[[:digit:]]{1,2}"
re_icd9_minor_e <- "[[:digit:]]{1}"

re_icd9_decimal_n_bare <- paste0(re_icd9_major_n, "(?:\\.(?:", re_icd9_minor_nv, ")?)?")
re_icd9_decimal_v_bare <- paste0(re_icd9_major_v, "(?:\\.(?:", re_icd9_minor_nv, ")?)?")
re_icd9_decimal_e_bare <- paste0(re_icd9_major_e, "(?:\\.(?:", re_icd9_minor_e, ")?)?")

re_icd9_decimal_n_strict_bare <- paste0(re_icd9_major_n_strict, "(?:\\.(?:", re_icd9_minor_nv, ")?)?")
re_icd9_decimal_v_strict_bare <- paste0(re_icd9_major_v_strict, "(?:\\.(?:", re_icd9_minor_nv, ")?)?")
re_icd9_decimal_e_strict_bare <- paste0(re_icd9_major_e_strict, "(?:\\.(?:", re_icd9_minor_e, ")?)?")

re_icd9_decimal_n <- re_icd9_decimal_n_bare %>% re_just_ws
re_icd9_decimal_v <- re_icd9_decimal_v_bare %>% re_just_ws
re_icd9_decimal_e <- re_icd9_decimal_e_bare %>% re_just_ws

re_icd9_short_n <- paste0(re_icd9_major_n, "(?:", re_icd9_minor_nv, ")?") %>% re_just_ws
re_icd9_short_v <- paste0(re_icd9_major_v, "(?:", re_icd9_minor_nv, ")?") %>% re_just_ws
re_icd9_short_e <- paste0(re_icd9_major_e, "(?:", re_icd9_minor_e, ")?") %>% re_just_ws

re_icd9_any_n <- paste0(re_icd9_major_n, "\\.?(?:", re_icd9_minor_nv, ")?") %>% re_just_ws
re_icd9_any_v <- paste0(re_icd9_major_v, "\\.?(?:", re_icd9_minor_nv, ")?") %>% re_just_ws
re_icd9_any_e <- paste0(re_icd9_major_e, "\\.?(?:", re_icd9_minor_e, ")?") %>% re_just_ws

re_icd9_decimal <- paste0(c(re_icd9_decimal_n,
                          re_icd9_decimal_v,
                          re_icd9_decimal_e),
                          collapse = "|")

re_icd9_decimal_bare <- paste0(c(re_icd9_decimal_n_bare,
                            re_icd9_decimal_v_bare,
                            re_icd9_decimal_e_bare),
                          collapse = "|")

re_icd9_decimal_strict_bare <- paste0(c(re_icd9_decimal_n_strict_bare,
                                        re_icd9_decimal_v_strict_bare,
                                        re_icd9_decimal_e_strict_bare),
                                      collapse = "|")

re_icd9_short <- paste0(c(re_icd9_short_n,
                          re_icd9_short_v,
                          re_icd9_short_e),
                        collapse = "|")

re_icd9_any <- paste0(c(re_icd9_any_n,
                          re_icd9_any_v,
                          re_icd9_any_e),
                        collapse = "|")


re_icd10cm_major_bare  <- "[[:alpha:]][[:digit:]][[:alnum:]]"
re_icd10who_major_bare <- "[[:alpha:]][[:digit:]][[:digit:]]"
re_icd10_major_bare <- re_icd10cm_major_bare # use slightly broader definition for generic
re_icd10cm_major  <- re_icd10cm_major_bare %>% re_just_ws
re_icd10who_major <- re_icd10who_major_bare %>% re_just_ws
re_icd10_major <- re_icd10_major_bare %>% re_just_ws

re_icd10cm_short <- paste0(re_icd10_major_bare, "[[:alnum:]]{0,4}")
re_icd10cm_decimal <- paste0(re_icd10_major_bare, "\\.[[:alnum:]]{0,4}")
re_icd10cm_any <- paste0(re_icd10_major_bare, "\\.?[[:alnum:]]{0,4}")

re_icd10_short <- re_icd10cm_short
re_icd10_decimal <- re_icd10cm_decimal
re_icd10_any <- re_icd10cm_any

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
#' @template whitespace_ok
#' @template dotdotdot
#' @seealso \url{http://www.stata.com/users/wgould/icd9/icd9.hlp}
#'   url{http://www.sascommunity.org/wiki/Validate_the_format_of_ICD-9_codes}
#' @family ICD9 validation
#' @return logical vector with \code{TRUE} or \code{FALSE} for each icd9 code
#'   provided according to its validity
#' @rawNamespace S3method(icd_is_valid,character)
#' @examples
#'   icd_is_valid(icd9(c("", "1", "22", "333", "4444", "123.45", "V",
#'                      "V2", "V34", "V567", "E", "E1", "E70", "E")))
#'   # internal function:
#'   \dontrun{
#'   icd:::icd_is_valid_major(c("", "1", "22", "333", "4444", "123.45", "V",
#'                      "V2", "V34", "V567", "E", "E1", "E70", "E"))
#'   }
#' @export
icd_is_valid <- function(x, ...) {
  UseMethod("icd_is_valid")
}

#' @describeIn icd_is_valid_major Test whether an ICD code is of major type,
#'   which at present assumes ICD-9 format
#' @export
#' @keywords internal
icd_is_valid.default <- function(x, ...) {
  stop("Specify type for checking validity of ICD-9 or ICD-10 codes, to avoid ambiguity, e.g. V10", call. = FALSE)
}

#' @describeIn icd_is_valid Test whether generic ICD-10 code is valid
#' @export
icd_is_valid.icd10 <- function(x, short_code = icd_guess_short(x),
                               whitespace_ok = TRUE, ...) {
  assert_character(x)
  assert_flag(short_code)
  assert_flag(whitespace_ok)

  # SOMEDAY: check whether code has 'year' attribute. This is maybe more for
  # testing 'realness' start with a broad regex

  # TODO: test whether icd-10-cm or WHO, if class not otherwise specified.
  if (short_code)
    x %>% str_trim() %>% str_detect(re_icd10_short)
  else
    x %>% str_trim() %>% str_detect(re_icd10_decimal)
}

#' @describeIn icd_is_valid Test whether generic ICD-10 code is valid
#' @export
icd_is_valid.icd9 <- function(x, short_code = icd_guess_short.icd9(x),
                              whitespace_ok = TRUE, ...) {
  assert(
    checkmate::checkFactor(x),
    checkCharacter(x),
    checkClass(x, c("icd9")), # TODO: use icd9_classes
    checkClass(x, c("icd9cm"))
  )
  assert_flag(short_code)
  assert_flag(whitespace_ok)
  if (short_code)
    icd9_is_valid_short(x, whitespace_ok = whitespace_ok)
  else
    icd9_is_valid_decimal(x, whitespace_ok = whitespace_ok)
}

# Test whether a character vector of ICD vodes is
#   valid, guessing both type and version of the ICD codes
#
# Can't roxygen because of UseMethod bug therein
icd_is_valid.character <- function(x, short_code = icd_guess_short(x),
                                   whitespace_ok = TRUE, ...) {
  assert_flag(whitespace_ok)
  x <- icd_guess_version_update(x)
  UseMethod("icd_is_valid", x)
}

icd9_is_valid_decimal <- function(x, whitespace_ok = TRUE) {
  assert(checkmate::checkFactor(x), checkCharacter(x),
         checkClass("icd9"), checkClass("icd9cm"))
  assert_flag(whitespace_ok)
  if (length(x) == 0)
    return(logical())

  if (whitespace_ok)
    str_detect(as_char_no_warn(x), re_just_ws(re_icd9_decimal)) %>% na_to_false
  else
    str_detect(as_char_no_warn(x), re_just(re_icd9_decimal)) %>% na_to_false
}

icd9_is_valid_short <- function(x, whitespace_ok = TRUE) {
  # if input doesn't satisfy these, then it is not just invalid, but deserves an error:
  assert(
    checkmate::checkFactor(x),
    checkCharacter(x),
    checkClass(x, c("icd9")),
    checkClass(x, c("icd9cm"))
  )
  assert_flag(whitespace_ok)

  # TODO: do this everywhere or nowhere
  if (length(x) == 0)
    return(logical())

  # as explained in details, a numeric short_code ID has different validity
  # requirements than a string because of leading zeroes.

  if (whitespace_ok)
    str_detect(as_char_no_warn(x), re_just_ws(re_icd9_short)) %>% na_to_false
  else
    str_detect(as_char_no_warn(x), re_just(re_icd9_short)) %>% na_to_false
  }

icd9_is_valid_short_n <- function(x, whitespace_ok = TRUE) {
  assert_flag(whitespace_ok)
  if (whitespace_ok)
    str_detect(as_char_no_warn(x), re_just_ws(re_icd9_short_n)) %>% na_to_false
  else
    str_detect(as_char_no_warn(x), re_just(re_icd9_short_n)) %>% na_to_false
}

icd9_is_valid_short_v <- function(x, whitespace_ok = TRUE) {
  assert_flag(whitespace_ok)
  if (whitespace_ok)
    str_detect(as_char_no_warn(x), re_just_ws(re_icd9_short_v)) %>% na_to_false
  else
    str_detect(as_char_no_warn(x), re_just(re_icd9_short_v)) %>% na_to_false
}

icd9_is_valid_short_e <- function(x, whitespace_ok = TRUE){
  assert_flag(whitespace_ok)
  if (whitespace_ok)
    str_detect(as_char_no_warn(x), re_just_ws(re_icd9_short_e)) %>% na_to_false
  else
    str_detect(as_char_no_warn(x), re_just(re_icd9_short_e)) %>% na_to_false
}

icd9_is_valid_decimal_n <- function(x, whitespace_ok = TRUE) {
  assert(checkmate::checkFactor(x), checkCharacter(x))
  assert_flag(whitespace_ok)
  if (whitespace_ok)
    str_detect(as_char_no_warn(x), re_just_ws(re_icd9_decimal_n)) %>% na_to_false
  else
    str_detect(as_char_no_warn(x), re_just(re_icd9_decimal_n)) %>% na_to_false
}

icd9_is_valid_decimal_v <- function(x, whitespace_ok = TRUE) {
  assert_flag(whitespace_ok)
  if (whitespace_ok)
    str_detect(as_char_no_warn(x), re_just_ws(re_icd9_decimal_v)) %>% na_to_false
  else
    str_detect(as_char_no_warn(x), re_just(re_icd9_decimal_v)) %>% na_to_false
}

icd9_is_valid_decimal_e <- function(x, whitespace_ok = TRUE) {
  assert_flag(whitespace_ok)
  #need Perl regex for lookbehind. may even be quicker, according to the docs.
  #grepl("^E(?!0+($|\\.))[[:digit:]][[:digit:]]{0,2}(\\.[[:digit:]]?)?$",
  #trim(x), perl = TRUE)
  assert(checkmate::checkFactor(x), checkCharacter(x))
  if (whitespace_ok)
    str_detect(as_char_no_warn(x), re_just_ws(re_icd9_decimal_e)) %>% na_to_false
  else
    str_detect(as_char_no_warn(x), re_just(re_icd9_decimal_e)) %>% na_to_false
}

#' @title Test whether an ICD code is major
#' @description codes without real or implied decimal place return TRUE
#' @param x vector of ICD codes
#' @return logical vector of same length as input, with TRUE when a code is a
#'   major (not necessarily a real one)
#' @keywords internal
icd_is_valid_major <- function(x, whitespace_ok = TRUE) {
  assert_flag(whitespace_ok)
  UseMethod("icd_is_valid_major")
}

#' @describeIn icd_is_valid_major Test whether an ICD code is of major type,
#'   which at present assumes ICD-9 format
#' @export
#' @keywords internal
icd_is_valid_major.default <- function(x, whitespace_ok = TRUE) {
  stop("Specify type for checking validity of ICD-9 or ICD-10 major codes, to avoid ambiguity, e.g. V10", call. = FALSE)
}

#' @describeIn icd_is_valid_major Test whether an ICD-9 code is of major type.
#' @export
#' @keywords internal
icd_is_valid_major.icd9 <- function(x, whitespace_ok = TRUE) {
  assert_flag(whitespace_ok)
  if (whitespace_ok)
    str_detect(as_char_no_warn(x), re_just(re_icd9_major)) %>% na_to_false
  else
    str_detect(as_char_no_warn(x), re_just(re_icd9_major_bare)) %>% na_to_false
}

#' @describeIn icd_is_valid_major Test whether an ICD-9 code is of major type.
#' @export
#' @keywords internal
icd_is_valid_major.icd10 <- function(x, whitespace_ok = TRUE) {
  assert_flag(whitespace_ok)
  if (whitespace_ok)
    str_detect(as_char_no_warn(x), re_just(re_icd10_major)) %>% na_to_false
  else
    str_detect(as_char_no_warn(x), re_just(re_icd10_major_bare)) %>% na_to_false
}

#' @rdname icd_is_valid_major
#' @keywords internal
icd9_is_valid_major_n <- function(x, whitespace_ok = TRUE) {
  assert_flag(whitespace_ok)
  if (whitespace_ok)
    str_detect(as_char_no_warn(x), re_just_ws(re_icd9_major_n)) %>% na_to_false
  else
    str_detect(as_char_no_warn(x), re_just(re_icd9_major_n)) %>% na_to_false
}

#' @rdname icd_is_valid_major
#' @keywords internal
icd9_is_valid_major_v <- function(x, whitespace_ok = TRUE) {
  assert_flag(whitespace_ok)
  if (whitespace_ok)
    str_detect(as_char_no_warn(x), re_just_ws(re_icd9_major_v)) %>% na_to_false
  else
    str_detect(as_char_no_warn(x), re_just(re_icd9_major_v)) %>% na_to_false
  }
#' @rdname icd_is_valid_major
#' @keywords internal
icd9_is_valid_major_e <- function(x, whitespace_ok = TRUE) {
  assert_flag(whitespace_ok)
  if (whitespace_ok)
    str_detect(as_char_no_warn(x), re_just_ws(re_icd9_major_e)) %>% na_to_false
  else
    str_detect(as_char_no_warn(x), re_just(re_icd9_major_e)) %>% na_to_false
}

#' @describeIn icd_is_valid Validate an icd9 mapping to comorbidities
#' @export
#' @keywords internal
icd_is_valid.icd_comorbidity_map <- function(x, short_code, ...) {
  assert_list(x, types = "character", any.missing = FALSE,
                        min.len = 1, unique = TRUE, names = "named")
  assert_flag(short_code)
  # TOOD: warn/return the invalid labels?
  all(unlist(
    lapply(x, FUN = function(y) icd_is_valid(y, short_code = short_code)),
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
#' @rawNamespace S3method(icd_get_invalid,default)
#' @export
icd_get_invalid <- function(...) {
  UseMethod("icd_get_invalid")
}

# @describeIn icd_get_invalid Default method when ICD version or short versus
#   decimal not known.
# @export
# @keywords internal
icd_get_invalid.default <- function(x, short_code = NULL, ...) {
  # both <- icd_guess_both(x, short_code = short_code)
  x %<>% icd_guess_short_update %>% icd_guess_version_update
  UseMethod("icd_get_invalid", x)
}

#' @describeIn icd_get_invalid Get invalid ICD-9 codes from vector of codes
#' @param x vector of ICD codes, or list of vectors of ICD codes forming a
#'   comorbidity map
#' @export
#' @keywords internal
icd_get_invalid.icd9 <- function(x, short_code = icd_guess_short.icd9(x), ...) {
  x[!icd_is_valid.icd9(x, short_code = short_code)]
}

#' @describeIn icd_get_invalid Get invalid ICD-10 codes from vector of codes
#' @export
#' @keywords internal
icd_get_invalid.icd10 <- function(x, short_code = icd_guess_short.icd10(x), ...) {
  # this seems like boilerplate code, but avoids infinite recursion
  x[!icd_is_valid.icd10(x, short_code = short_code)]
}

#' @describeIn icd_get_invalid Get invalid elements of a comorbidity map
#' @export
#' @keywords internal
icd_get_invalid.icd_comorbidity_map <- function(x, short_code = icd_guess_short(x), ...) {
  # todo: may need to switch on ICD code type
  class(x) <- class(x)[class(x) != "icd_comorbidity_map"]
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

#' @export
#' @keywords internal
icd_is_major.default <- function(x) {
  y <- icd_guess_version_update(x)
  icd_is_major(y)
}

#' @describeIn icd_is_major check whether a code is an ICD-10 major
#' @export
#' @keywords internal
icd_is_major.icd10 <- function(x) {
  assert_character(x)
  # if not know whether ICD-10-CM, then use broader definition
  icd_is_major.icd10cm(x)
}

#' @describeIn icd_is_major check whether a code is an ICD-10-CM major.
#'   Currently uses \code{stringr} which uses \code{stringi} which should be
#'   quite fast, but does suffer from handling unicode, locales, etc.
#' @export
#' @keywords internal
icd_is_major.icd10cm <- function(x) {
  assert_character(x)
  str_detect(x, re_just_ws(re_icd10cm_major))
}

#' @describeIn icd_is_major check whether a code is an ICD-10 WHO major
#' @export
#' @keywords internal
icd_is_major.icd10who <- function(x) {
  assert_character(x)
  str_detect(x, re_just_ws(re_icd10who_major))
}

#' @describeIn icd_is_major check whether a code is an ICD-9 major
#' @export
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
  # icd9IsA(as_char_no_warn(x), "VEve", TRUE)
  icd9_is_n_cpp(as_char_no_warn(x))
}

#' @describeIn icd9_is_n are the given codes V type?
#' @export
icd9_is_v <- function(x) {
  icd9_is_v_cpp(as_char_no_warn(x))
  #icd9IsA(as_char_no_warn(x), "Vv", FALSE)
}

#' @describeIn icd9_is_n are the given codes E type?
#' @export
icd9_is_e <- function(x) {
  icd9_is_e_cpp(as_char_no_warn(x))
  #icd9IsA(as_char_no_warn(x), "Ee", FALSE)
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
  assert_character(x)
  assert_flag(short_code)
  # SOMEDAY: check whether code has 'year' attribute. This is maybe more for
  # testing whether a code is defined for a particular set of ICD codes, e.g. an
  # annual revision of ICD-10-CM

  #  start with a broad regex

  str_trim(x) %>%
    str_detect("^[[:alpha:]][[:digit:]][[:digit:]]\\.?(X|[[:digit:]]*)$")

}
