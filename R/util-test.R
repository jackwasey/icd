# Copyright (C) 2014 - 2016  Jack O. Wasey
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

#' expect equal, ignoring any ICD classes
#' @keywords internal
expect_equal_no_icd <- function(object, expected, ..., info = NULL,
                                label = NULL, expected.label = NULL) {
  if (is.null(label))
    label <- find_expr("object")
  if (is.null(expected.label))
    expected.label <- find_expr("expected")

  class(object) <- class(object)[class(object) %nin% icd_all_classes]
  eval(bquote(testthat::expect_that(.(object),
                                    testthat::equals(
                                      .(expected), label = expected.label, ...
                                    ), info = info, label = label)))
}

#' expect named sub-chapter has a given range, case insensitive
#' @keywords internal
expect_chap_equal <- function(x, start, end, ver_chaps, ...) {
  x <- tolower(x)
  lower_case_names <- tolower(names(ver_chaps))
  res <- eval(bquote(testthat::expect_true(.(x) %in% lower_case_names, ...)))
  if (!res$passed)
    return(res)

  t_i <- which(lower_case_names == x)
  eval(bquote(testthat::expect_equal(.(ver_chaps[[t_i]]), c(start = .(start), end = .(end)), ...)))
}

#' @rdname expect_chap_equal
expect_icd10_chap_equal <- function(x, start, end, ...) {
  eval(bquote(expect_chap_equal(.(x), start, end, ver_chaps = icd9::icd10_chapters, ...)))
}

#' @rdname expect_chap_equal
expect_icd10_sub_chap_equal <- function(x, start, end, ...) {
  eval(bquote(expect_chap_equal(.(x), .(start), .(end), ver_chaps = icd9::icd10_sub_chapters, ...)))
}

#' @rdname expect_chap_equal
expect_icd9_chap_equal <- function(x, start, end, ...) {
  eval(bquote(expect_chap_equal(.(x), start, end, ver_chaps = icd9::icd9_chapters, ...)))
}

#' @rdname expect_chap_equal
expect_icd9_sub_chap_equal <- function(x, start, end, ...) {
  eval(bquote(expect_chap_equal(.(x), .(start), .(end), ver_chaps = icd9::icd9_sub_chapters, ...)))
}

expect_icd9_major_equals <- function(x, code, ...) {
  res <- eval(bquote(expect_true(.(x) %in% names(icd9_majors), ...)))
  if (res$passed)
    eval(bquote(expect_equal(icd9_majors[[.(x)]], .(code), ...)))
  else
    res
}

expect_icd9_major_is_sub_chap <- function(x, code, ...) {
  res <- eval(bquote(expect_icd9_sub_chap_equal(.(x), start = .(code), end = .(code), ...)))
  if (res$passed)
    eval(bquote(expect_icd9_major_equals(.(x), .(code), ...)))
  else
    res
}

chap_missing <- function(ver_chaps, ...) {
  function(x) {
    x <- tolower(x)
    lnames <- tolower(names(ver_chaps))
    testthat::expectation(x %nin% lnames,
                          "should not be present",
                          "is correctly missing")
  }
}

chap_present <- function(ver_chaps, ...) {
  function(x) {
    x <- tolower(x)
    lnames <- tolower(names(ver_chaps))
    testthat::expectation(x %in% lnames,
                          "should not be missing",
                          "is present")
  }
}

expect_chap_missing <- function(x, ver_chaps, info = NULL, label = NULL, ...) {
  eval(bquote(expect_that(.(x), chap_missing(ver_chaps = ver_chaps, ...),
                          info = info, label = label)))
}

expect_icd9_sub_chap_missing <- function(x, ...) {
  eval(bquote(expect_chap_missing(.(x), ver_chaps = icd9::icd9_sub_chapters, ...)))
}

expect_icd9_chap_missing <- function(x, ...) {
  eval(bquote(expect_chap_missing(.(x), ver_chaps = icd9::icd9_chapters, ...)))
}

expect_icd10_sub_chap_missing <- function(x, ...) {
  eval(bquote(expect_chap_missing(.(x), ver_chaps = icd9::icd10_sub_chapters, ...)))
}

expect_icd10_chap_missing <- function(x, ...) {
  eval(bquote(expect_chap_missing(.(x), ver_chaps = icd9::icd10_chapters, ...)))
}

#' expect that a chapter with given title exists, case-insensitive
#' @keywords internal
expect_chap_present <- function(x, ver_chaps, info = NULL, label = NULL, ...) {
  eval(bquote(expect_that(.(x), chap_present(ver_chaps = ver_chaps, ...),
                          info = info, label = label)))
}

#' @rdname expect_chap_present
#' @keywords internal
expect_icd9_sub_chap_present <- function(x, info = NULL, label = NULL, ...) {
  eval(bquote(expect_chap_present(.(x), ver_chaps = icd9::icd9_sub_chapters,
                                  info = info, label = label, ...)))
}

#' @rdname expect_chap_present
#' @keywords internal
expect_icd9_chap_present <- function(x, ...) {
  eval(bquote(expect_chap_present(.(x), ver_chaps = icd9::icd9_chapters, ...)))
}

#' @rdname expect_chap_present
#' @keywords internal
expect_icd10_sub_chap_present <- function(x, ...) {
  eval(bquote(expect_chap_present(.(x), ver_chaps = icd9::icd10_sub_chapters, ...)))
}

#' @rdname expect_chap_present
#' @keywords internal
expect_icd10_chap_present <- function(x, ...) {
  eval(bquote(expect_chap_present(.(x), ver_chaps = icd9::icd10_chapters, ...)))
}

expect_icd9_only_chap <- function(x, ...) {
  res <- eval(bquote(expect_icd9_chap_present(.(x), ...)))
  if (res$passed)
    eval(bquote(expect_icd9_sub_chap_missing(.(x), ...)))
  else
    res
}

expect_icd9_only_sub_chap <- function(x, info = NULL, label = NULL, ...) {
  res <- eval(bquote(expect_icd9_sub_chap_present(.(x), info = info, label = label, ...)))
  if (res$passed)
    eval(bquote(expect_icd9_chap_missing(.(x), info = info, label = label, ...)))
  else
    res
}

expect_icd10_only_chap <- function(x, ...) {
  res <- eval(bquote(expect_icd10_chap_present(.(x), ...)))
  if (res$passed)
    eval(bquote(expect_icd10_sub_chap_missing(.(x), ...)))
  else
    res
}

expect_icd10_only_sub_chap <- function(x, ...) {
  res <- eval(bquote(expect_icd10_sub_chap_present(.(x), ...)))
  if (res$passed)
    eval(bquote(expect_icd10_chap_missing(.(x), ...)))
  else
    res

}

#' expectation for ICD classes to be well-ordered
#' @return a function, mimicking how \code{testthat} works
#' @examples
#'   testthat::expect_that(icd9("V10"), icd9:::icd_classes_are_ordered())
#' @keywords internal
icd_classes_are_ordered <- function() {
  function(x) {
    testthat::expectation(all(icd_classes_ordered(x)),
                          "are not well ordered", "are well ordered")
  }
}
