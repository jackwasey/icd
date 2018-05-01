# Copyright (C) 2014 - 2018  Jack O. Wasey
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

#' Get or set whether ICD codes have have an attribute indicating 'short' or
#' 'decimal' format
#'
#' @section Getting the attribute: \code{is.short_diag} tests for presence of an attribute, not whether the code
#' is a valid ICD code. If \code{must_work} is \code{TRUE} then \code{NULL}
#' (i.e. no attribute set) returns \code{FALSE}, otherwise \code{NULL} is
#' returned. To test validity, see \code{\link{is_valid}}.
#' @section Setting the attribute: Similarly, \code{as.icd_short_diag} and \code{as.icd_decimal_diag} set the
#' attribute, but do not convert the codes themselves. For conversion between
#' 'short' and 'decimal' forms, use \code{\link{decimal_to_short}} and
#' \code{\link{short_to_decimal}}.
#'
#' The attribute \code{icd_short_code} should be either \code{TRUE} or
#' \code{FALSE}. There is no attribute named \code{icd_decimal_code}. These
#' functions set and get the attribute safely.
#' @param x ICD data
#' @param value \code{TRUE} or \code{FALSE}, default is \code{TRUE} which sets
#'   the attribute to whatever is indicated in the function name. See examples.
#' @param must_work single logical value, if \code{FALSE} (the default) this may
#'   return \code{NULL} if the attribute is not present. If \code{TRUE}, then
#'   either \code{TRUE} or \code{FALSE} is returned.
#' @examples
#' as.icd_short_diag("6670")
#'
#' as.icd_short_diag("667.0") # no warning or error!
#'
#' is.icd_short_diag(decimal_to_short("667.0"))
#'
#' decimal_type_code <- as.icd_short_diag("667.0", FALSE)
#' stopifnot(is.icd_decimal_diag(decimal_type_code))
#'
#' codes <- as.icd9(c("100.1", "441.3"))
#' codes <- as.decimal_diag(codes)
#' codes
#' @export
is.icd_short_diag <- function(x, must_work = FALSE) {
  res <- attr(x, "icd_short_diag", exact = TRUE)
  if (!must_work)
    res
  else
    isTRUE(res)
}

#' @rdname is.short_diag
#' @export
is.icd_decimal_diag <- function(x, must_work = FALSE) {
  res <- attr(x, "icd_short_diag", exact = TRUE)
  if (!must_work && is.null(res))
    NULL
  else
    identical(res, FALSE)
}

#' @rdname is.short_diag
#' @export
is.short_diag <- function(x, must_work = FALSE)
  is.icd_short_diag(x, must_work = must_work)

#' @rdname is.short_diag
#' @export
is.decimal_diag <- function(x, must_work = FALSE)
  is.icd_decimal_diag(x, must_work = must_work)

#' @rdname is.short_diag
#' @export
as.icd_short_diag <- function(x, value = TRUE) {
  checkmate::assert_flag(value)
  attr(x, "icd_short_diag") <- value
  x
}

#' @rdname is.short_diag
#' @export
as.icd_decimal_diag <- function(x, value = TRUE) {
  checkmate::assert_flag(value)
  attr(x, "icd_short_diag") <- !value
  x
}

#' @rdname is.short_diag
#' @export
as.short_diag <- function(x, value = TRUE)
  as.icd_short_diag(x, value)

#' @rdname is.short_diag
#' @export
as.decimal_diag <- function(x, value = TRUE)
  as.icd_decimal_diag(x, value)
