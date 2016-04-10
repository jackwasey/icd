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

#' Sort short-form ICD-9 codes
#'
#' Sorts lists of numeric only, V or E codes. Note that a simple numeric sort
#' does not work for ICD-9 codes, since "162" > "1620", and also V codes precede
#' E codes.
#' @details Implementation used fast built-in sort, then shuffles the E codes to
#'   the end.
#' @param x vector of ICD codes to sort
#' @template short_code
#' @template dotdotdot
#' @return sorted vector of ICD-9 codes. Numeric, then E codes, then V codes.
#' @keywords manip
#' @export
icd_sort <- function(x, ...)
  UseMethod("icd_sort")

#' @describeIn icd_sort Guess whether ICD-9 or ICD-10 (or possibly sub-type in
#'   the future) then sort based on that type. ICD-10 codes, note that setting
#'   \code{short} is unnecessary and ignored.
#' @export
#' @keywords internal
icd_sort.default <- function(x, short_code = icd_guess_short(x), ...) {
  switch(
    icd_guess_version(x, short_code = short_code),
    "icd9" = icd_sort.icd9(x, short_code),
    "icd10" = icd_sort.icd10(x, short_code),
    stop("ICD version not known")
  )
}

#' @describeIn icd_sort Sort ICD-10 codes, note that setting \code{short} is
#'   unnecessary and ignored.
#' @keywords internal
#' @export
icd_sort.icd10 <- function(x, short_code = NULL, ...) {
  # ignore short, it doesn't matter
  sort(x)
}

#' @describeIn icd_sort sort ICD-9 codes respecting numeric, then 'V', then 'E'
#'   codes, and accounting for leading zeroes. Will return a factor if a factor is given.
#' @keywords internal
#' @export
icd_sort.icd9 <- function(x, short_code = icd_guess_short(x), ...) {
  assert(checkmate::checkFactor(x), checkmate::checkCharacter(x))
  assert_flag(short_code)

  y <- if (short_code)
    icd9_add_leading_zeroes(x, short_code = short_code)
  else
    icd_decimal_to_short.icd9(x)

  res <- if (is.factor(x))
    x[icd9_order_cpp(as_char_no_warn(y))]
  else
    x[icd9_order_cpp(y)]

  class(res) <- class(x)
  attributes(res) <- attributes(x)
  res
}
#' Get order of short-form ICD-9 codes
#'
#' Puts E codes after V codes. \code{NA} values can't be ordered and are dropped
#' with a warning if found.
#' @param x vector or factor of ICD-9 codes
#' @examples
#' x <- icd:::generate_random_decimal_icd9(1e4)
#' system.time(icd:::icd9_sort_cpp(x)) # vastly quicker
#' system.time(icd:::icd9_order_short(x))
#' \dontrun{
#' # fastmatch is fractionally faster, but either is very slow
#' library(microbenchmark)
#' microbenchmark(icd9_order_short(x), icd9_order_short_fm(x), icd9_order_short_r(x), times = 10)
#' # C++ method (which also ignores NA values) is 100x faster.
#' }
#' @return vector of integers with length of the non-NA values in \code{x}
#' @keywords internal
icd9_order_short <- function(x) {
  if (anyNA(x)) {
    warning("can't order NA values, so dropping them")
    x <- x[!is.na(x)]
  }
  icd9_order_cpp(x)
}

icd9_order_short_fm <- function(x) {
  y <- x[order(icd9_add_leading_zeroes(x, short_code = TRUE))]
  fmatch(
    y[c(which(icd9_is_n(y)), which(icd9_is_v(y)), which(icd9_is_e(y)))],
    x)
}

icd9_order_short_r <- function(x) {
  y <- x[order(icd9_add_leading_zeroes(x, short_code = TRUE))]
  match(
    y[c(which(icd9_is_n(y)), which(icd9_is_v(y)), which(icd9_is_e(y)))],
    x)
}
