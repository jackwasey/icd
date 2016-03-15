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

#' guess whether codes are \code{short_code} or \code{decimal_code}
#'
#' TODO: Partly implemented. Goal is to guess whether codes are
#' \code{short_code} or \code{decimal_code} form. Currently condense works, but
#' not with the \code{icd} look-up table currently in use. Of note, validation
#' is a bit different here, since we don't know the type until after we guess.
#' We could look for where both short_code and long are invalid, and otherwise
#' assume valid, even if the bulk are short_code. However, it may be more useful
#' to check validity after the guess.
#' @return single logical value, \code{TRUE} if input data are predominantly
#'   \code{short_code} type. If there is some uncertainty, then return
#'   \code{NA}.
#' @keywords internal
icd_guess_short <- function(x, short_code = NULL, test_n = 1000L, ...) {
  # if short_code is set, no need to dispatch at all
  if (!is.null(short_code)) 
    return(short_code)
  if (is.icd_short_code(x))
    return(TRUE)
  if (is.icd_decimal_code(x))
    return(FALSE)
  UseMethod("icd_guess_short")
}

#' @describeIn icd_guess_short Guess whether a data frame has ICD-9 or ICD-10
#'   codes
#' @export
#' @method icd_guess_short data.frame
#' @keywords internal
icd_guess_short.data.frame <- function(x, short_code = NULL, test_n = 1000L, icd_name = get_icd_name(x)) {
  UseMethod("icd_guess_short", x[[icd_name]]) 
}

.guess_warn <- function() {
  message("gw")
  opt <- getOption("icd.warn_guess_short")
  if (is.null(opt))
    return()
  assert_flag(opt)
  if (opt)
    warning("all codes used for guessing are 'major' part, i.e. before decimal place would be, ",
            "therefore unable to guess whether codes are 'short' or 'decimal'. ",
            "The default is to assume 'short'.")
}

# this works when the type of 'x' is known, icd9 vs icd10 
.guess_short <- function(x, short_code, test_n) {
  if (is.list(x))
    x <- unlist(x, recursive = TRUE)
  x <- as_char_no_warn(x) # preserves class (except factor)
  testend <- min(length(x), test_n)
  vm <- icd_is_valid_major(x[1:testend])
  if (all(vm, na.rm = TRUE)) {
    .guess_warn()
    return(TRUE)
  }
  vs <- icd_is_valid(x[1:testend], short_code = TRUE) & !vm
  vd <- icd_is_valid(x[1:testend], short_code = FALSE) & !vm
  sum(vd, na.rm = TRUE) <= sum(vs, na.rm = TRUE)
}

#' @describeIn icd_guess_short Guess whether an ICD-9 code is in short_code form
#' @export
#' @keywords internal
icd_guess_short.icd9 <- function(x, short_code = NULL, test_n = 1000L, ...) {
  .guess_short(x, short_code, test_n)
}

#' @describeIn icd_guess_short Guess short when ICD-10 type is known
#' @export
#' @keywords internal
icd_guess_short.icd10 <- function(x, short_code = NULL, test_n = 1000L, ...) {
  .guess_short(x, short_code, test_n)
}

#' @describeIn icd_guess_short Guess short when ICD-10 type is known
#' @export
#' @keywords internal
icd_guess_short.icd10cm <- function(x, short_code = NULL, test_n = 1000L, ...) {
  .guess_short(x, short_code, test_n)
}

#' @describeIn icd_guess_short Guess short from a list
#' @export
#' @method icd_guess_short list
#' @keywords internal
icd_guess_short.list <- function(x, short_code = NULL, test_n = 1000L) {
  UseMethod("icd_guess_short", unlist(x, recursive = TRUE))
}

#' @describeIn icd_guess_short Guess short naive default method
#' @export
#' @keywords internal
icd_guess_short.default <- function(x, short_code = NULL, test_n = 1000L, ...) {

  # if all the codes are major, we should warn the user
  .guess_short(x, short_code, test_n)
}

#' @describeIn icd_guess_short Guess short when type is short
#' @export
#' @keywords internal
icd_guess_short.icd_short_code <- function(x) TRUE #nocov

#' @describeIn icd_guess_short Guess short when type is decimal
#' @export
#' @keywords internal
icd_guess_short.icd_decimal_code <- function(x) FALSE #nocov

#' Guess version of ICD codes
#'
#' The guess is indeed a guess and can be wrong. There are some codes which
#' could be either ICD-9 or ICD-10. The current implementation doesn't check
#' whether the codes exist in any definitions (ICD-9 CM or WHO, for example),
#' just whether they are valid. Thus it is quicker.
#'
#' @details TODO: consider adding warning depending on degree of uncertainty.
#'
#' @param x input data
#' @template short_code
#' @export
#' @keywords internal
icd_guess_version <- function(x, short_code, ...)
  UseMethod("icd_guess_version")

#' @describeIn icd_guess_version Guess version class ICD-9 codes
#' @export
#' @keywords internal
icd_guess_version.icd9 <- function(x, short_code, ...) "icd9" #nocov

#' @describeIn icd_guess_version Guess version of class ICD-10 codes
#' @export
#' @keywords internal
icd_guess_version.icd10 <- function(x, short_code, ...) "icd10" #nocov

#' @describeIn icd_guess_version Guess version of ICD codes in a factor
#' @export
#' @keywords internal
icd_guess_version.factor <- function(x, short_code = NULL, ...) {
  icd_guess_version.character(as_char_no_warn(x), short_code = short_code, ...)
}

#' @describeIn icd_guess_version Guess version of ICD codes in character vector
#' @export
#' @keywords internal
icd_guess_version.character <- function(x, short_code = NULL, ...) {
  # TODO: this is too complicated. The short test can really just be looking for
  # the decimal place, maybe in C/C++, but doesn't need anything special extra,
  # and I can't see how ICD version would make a difference
  assert_character(x)
  assert(checkmate::checkFlag(short_code), checkmate::checkNull(short_code))
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

  # todo: guess ICD-10-CM, if there is any ICD-10-CM code amongst other ICD-10 codes?
  # TODO: return vector of types, e.g. c("icd10who, "icd10")

  if (i9 >= i10 && i9 >= i10who)
    "icd9"
  else
    "icd10"
}

#' @describeIn icd_guess_version Guess version of ICD codes in a field in a
#'   \code{data.frame}
#' @keywords internal
#' @export
icd_guess_version.data.frame <- function(x, short_code = NULL, icd_name = get_icd_name(x), ...) {
  assert_data_frame(x)
  icd_guess_version.character(as_char_no_warn(x[[icd_name]]))
}

#' Guess version of ICD and update class
#'
#' Guesses the version of given ICD codes and sets the class of the
#'   returned data according to the guess.
#' @param x vector of ICD codes of some type
#' @template short_code
#' @return the input data with appropriate ICD class set
#' @keywords internal
icd_guess_version_update <- function(x, short_code = icd_guess_short(x)) {
  # could either return a method from the guess version function (nice and
  # functional), use the returned string as a function name to invoke, or switch
  # on the string. Just adding the class is bad, e.g. would miss icd10cm if
  # added
  ver <- icd_guess_version(x, short_code)
  if (ver == "icd9")
    icd9(x)
  else if (ver == "icd10")
    icd10(x)
  else
    stop("unknown type returned when guessing ICD version")
}

#' Guess short vs decimal of ICD and update class
#'
#' Guesses whether the given ICD codes are short or long format, and set the
#' class of the returned data according to the guess.
#' @return the input data with appropriate ICD class set
#' @keywords internal
icd_guess_short_update <- function(x, icd_name = get_icd_name(x),
                                   short_code = icd_guess_short(x)) {
  if (short_code)
    icd_short_code(x)
  else
    icd_decimal_code(x)
}

#' Guess the ICD version (9 or 10) from a pair of codes
#'
#' Designed for guessing version of codes given to delineate a range
#' @param start ICD code
#' @param end ICD code
#' @template short_code
#' @return Type of codes as single character \code{"icd9"} or \code{"icd10"}, or
#'   error if conflicting results
#' @keywords internal
icd_guess_pair_version <- function(start, end, short_code = NULL) {
  start_guess <- icd_guess_version.character(as_char_no_warn(start), short_code = short_code)
  end_guess <- icd_guess_version.character(as_char_no_warn(end), short_code = short_code)
  if (start_guess != end_guess)
    stop("Cannot expand range because ICD code version cannot be guessed from ", start,
         " and ", end, ". Either specify the classes, e.g. icd9(\"100.4\"), or call the
       S3 method directly, e.g. icd_expand_range.icd9")
  start_guess
}
