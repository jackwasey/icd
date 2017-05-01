# Copyright (C) 2014 - 2017  Jack O. Wasey
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

#' @rdname is.icd9
#' @details \code{is.icd_short_diag} tests for presence of an attribute, not
#'   whether the code is a valid ICD code. If \code{must_work} is \code{TRUE}
#'   then \code{NULL} (i.e. no attribute set) returns \code{FALSE}, otherwise
#'   \code{NULL} is returned.
#' @param must_work single logical value, if \code{FALSE} (the default) this may
#'   return \code{NULL} if the attribute is not present. If \code{TRUE}, then
#'   either \code{TRUE} or \code{FALSE} is returned.
#' @export
is.icd_short_diag <- function(x, must_work = FALSE) {
  res <- attr(x, "icd_short_diag", exact = TRUE)
  if (!must_work)
    res
  else
    isTRUE(res)
}

#' @rdname is.icd9
#' @export
is.icd_decimal_diag <- function(x, must_work = FALSE) {
  res <- attr(x, "icd_short_diag", exact = TRUE)
  if (!must_work && is.null(res))
    NULL
  else
    identical(res, FALSE)
}

#' Change whether ICD code has short or long attribute
#'
#' The attribute \code{icd_short_code} should be either \code{TRUE} or
#' \code{FALSE}.
#' @param x ICD data
#' @param value True or false, default is \code{TRUE}
#' @examples
#' codes <- as.icd9(c("100.1", "441.3"))
#' codes <- as.icd_decimal_diag(codes)
#' codes
#' @export
as.icd_short_diag <- function(x, value = TRUE) {
  checkmate::assert_flag(value)
  attr(x, "icd_short_diag") <- value
  x
}

#' @rdname as.icd_short_diag
#' @export
as.icd_decimal_diag <- function(x, value = TRUE) {
  checkmate::assert_flag(value)
  attr(x, "icd_short_diag") <- !value
  x
}
