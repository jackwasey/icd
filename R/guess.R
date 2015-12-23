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

#' @title guess whether short_code or long
#' @description partly implemented. Goal is to guess whether codes are short_code or
#'   decimal form, then to call icd9Explain with the condense argument.
#'   Currently condense works, but not with the icd9 lookup table currently in
#'   use. Not exporting this function until it works as intended. Of note,
#'   validation is a bit different here, since we don't know the type until
#'   after we guess. We could look for where both short_code and long are invalid,
#'   and otherwise assume valid, even if the bulk are short_code. However, it may be
#'   more useful to check validity after the guess.
#' @return single logical value, \code{TRUE} if input data are predominantly
#'   short_code type. If there is some uncertainty, then return NA.
#' @keywords internal
icd_guess_short <- function(x, short_code = NULL, test_n = 1000L) {
  # if short_code is set, no need to dispatch at all
  if (!is.null(short_code)) {
    if (short_code)
      return(TRUE)
    else
      return(FALSE)
  }
  if (inherits(x, "icd_short_code")) return(TRUE)
  if (inherits(x, "icd_decimal_code")) return(FALSE)
  UseMethod("icd_guess_short")
}

#' @describeIn icd_guess_short Guess whether an ICD-9 code is in short_code form
#' @keywords internal
#' @export
icd_guess_short.icd9 <- function(x, short_code = NULL, test_n = 1000L) {
  if (inherits(x, "icd_short_code")) return(TRUE)
  if (inherits(x, "icd_decimal_code")) return(FALSE)
  if (!is.null(short_code)) {
    if (short_code)
      return(TRUE)
    else
      return(FALSE)
  }
  if (is.list(x)) x <- unlist(x, recursive = TRUE)
  x <- asCharacterNoWarn(x)
  testend <- min(length(x), test_n)
  vs <- icd9_is_valid_short(x[1:testend])
  vd <- icd9_is_valid_decimal(x[1:testend])
  sum(vd) <= sum(vs)
}

#' @export
icd_guess_short.icd10 <- function(x, short_code = NULL, test_n = 1000L) {
  # would be better to dispatch again with NextMethod, but that was crooked.
  icd_guess_short.default(x, short_code, test_n)
}

#' @export
icd_guess_short.list <- function(x, short_code = NULL, test_n = 1000L) {
  y <- unlist(x)
  icd_guess_short(y, short_code = short_code, test_n)
}

#' @export
icd_guess_short.default <- function(x, short_code = NULL, test_n = 1000L) {
  # any decimal as first approximation
  !any(stringr::str_detect(x[1:test_n], ".+\\..+"), na.rm = TRUE)
}

#' @export
icd_guess_short.icd_short_code <- function(x) TRUE

#' @export
icd_guess_short.icd_decimal_code <- function(x) FALSE

#' Guess version of ICD
#'
#' @keywords internal
#' @export
icd_guess_version <- function(x, ...)
  UseMethod("icd_guess_version")

#' @export
icd_guess_version.icd9 <- function(x, short_code, ...) "icd9"

#' @export
icd_guess_version.icd10 <- function(icd, short_code, ...) "icd10"

#' @describeIn icd_guess_version Guess version of ICD code from character vector
#' @export
icd_guess_version.character <- function(x, short_code = NULL, ...) {
  assertCharacter(x)
  if (!is.null(short_code)) {
    if (short_code) {
      i9 <- sum(icd_is_valid.icd9(x, short_code = TRUE), na.rm = TRUE)
      i10 <- sum(icd_is_valid.icd10(x, short_code = TRUE), na.rm = TRUE)
      i10who <- sum(icd_is_valid.icd10who(x, short_code = TRUE), na.rm = TRUE)
    } else {
      i9 <- sum(icd_is_valid.icd9(x, short_code = FALSE), na.rm = TRUE)
      i10 <- sum(icd_is_valid.icd10(x, short_code = FALSE), na.rm = TRUE)
      i10who <- sum(icd_is_valid.icd10who(x, short_code = FALSE), na.rm = TRUE)
    }

  } else {
    i9 <- max(
      sum(icd_is_valid.icd9(x, short_code = TRUE), na.rm = TRUE),
      sum(icd_is_valid.icd9(x, short_code = FALSE), na.rm = TRUE)
    )
    i10 <- max(
      sum(icd_is_valid.icd10(x, short_code = TRUE), na.rm = TRUE),
      sum(icd_is_valid.icd10(x, short_code = FALSE), na.rm = TRUE)
    )
    i10who <- max(
      sum(icd_is_valid.icd10who(x, short_code = TRUE), na.rm = TRUE),
      sum(icd_is_valid.icd10who(x, short_code = FALSE), na.rm = TRUE)
    )
  }

  #icd10cm <- sum(icd_is_valid.icd10cm(icd))

  # todo: guess ICD-10-CM
  # TODO: return vector of types, e.g. c("icd10who, "icd10")

  if (i9 >= i10 && i9 >= i10who)
    "icd9"
  else
    "icd10"
}

icd_guess_version.data.frame <- function(x, icd_name = get_icd_name(x), ...) {
  assertDataFrame(x)
  icd_guess_version.character(x[[icd_name]])
}

#' @title Guess version of ICD and update class
#' @description Guesses the version of given ICD codes and sets the class of the returned data according to the guess.
#' @return the input data with appropriate ICD class set
#' @keywords internal
icd_guess_version_update <- function(x) {
  class(x) <- append(icd_guess_version(x), class(x))
  x
}

#' @title Guess short vs decimal of ICD and update class
#' @description Guesses whether the given ICD codes are short or long format, and set the class of the returned data according to the guess.
#' @return the input data with appropriate ICD class set
#' @keywords internal
icd_guess_short_update <- function(x, icd_name = get_icd_name(x), short_code = NULL) {

  if (icd_guess_short(x, short_code = short_code))
    class(x) <- append("icd_short_code", class(x))
  else
    class(x) <- append("icd_decimal_code", class(x))
  x
}

#' Guess both ICD-9 vs ICD-10 or subtype, and whether short of long type.
#'
#' I think these are not quite orthogonal problems, so although this seems like
#' repetition, one may influence the other. Need to think about this.
#' @keywords internal
icd_guess_both <- function(x, short_code = NULL) {
  UseMethod("icd_guess_both")
}

#' @describeIn icd_guess_both default method which converts input to character
#' @keywords internal
icd_guess_both.default <- function(x, short_code = NULL) {
  x <- asCharacterNoWarn(x)
  ver <- icd_guess_version.character(x)
  class(x) <- append(ver, class(x))
  # now i've set the version I think it is, dispatch on this class to get short vs decimal
  short_code <- icd_guess_short(x)
  list(ver, short_code)
}
