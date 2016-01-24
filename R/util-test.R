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
  eval(bquote(testthat::expect_that(object,
                                    testthat::equals(
                                      expected, label = expected.label, ...
                                    ), info = info, label = label)))
}

#' expect named sub-chapter has a given range, case insensitive
#' @keywords internal
expect_chap_equal <- function(x, start, end, ver_chaps, ...) {
  x <- tolower(x)
  lnames <- tolower(names(ver_chaps))
  if (x %nin% lnames)
    eval(bquote(testthat::fail(paste(x, " not defined in (sub-)chapter names"))))
  t_i <- which(tolower(names(ver_chaps)) == x)
    r <- ver_chaps[[t_i]]
  eval(bquote(testthat::expect_equal(r, c(start = start, end = end), ...)))
}

#' @rdname expect_chap_equal
expect_icd10_chap_equal <- function(x, start, end, ...) {
  eval(bquote(expect_chap_equal(x, start, end, icd9::icd10_chapters, ...)))
}

#' @rdname expect_chap_equal
expect_icd9_subchap_equal <- function(x, start, end, ...) {
  eval(bquote(expect_chap_equal(x, start, end, icd9::icd9_sub_chapters, ...)))
}

#' @rdname expect_chap_equal
expect_icd9_chap_equal <- function(x, start, end, ...) {
  eval(bquote(expect_chap_equal(x, start, end, icd9::icd9_chapters, ...)))
}

chap_missing <- function() {
  function(x, ver_chaps, ...) {
    x <- tolower(x)
    lnames <- tolower(names(ver_chaps))
    test_that::expectation(x %in% lnames, "should not be present", "is correctly missing")
  }
}

expect_icd9_subchap_missing <- function(x, ...) {
  eval(bquote(expect_chap_missing(x, ver_chaps = icd9::icd9_sub_chapters, ...)))
}

#' expectation for ICD classes to be well-ordered
#' @return a function, mimicking how \code{testthat} works
#' @examples expect_that(icd9("V10"), icd_classes_are_ordered())
#' @keywords internal
icd_classes_are_ordered <- function() {
  function(x) {
    testthat::expectation(all(icd_classes_ordered(x)),
                "are not well ordered", "are well ordered")
  }
}
