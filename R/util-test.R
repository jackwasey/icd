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

skip_slow_tests <- function(msg = "skipping slow test") {
  do_slow_tests <- getOption("icd.do_slow_tests")
  if (is.null(do_slow_tests) || !do_slow_tests)
    testthat::skip(msg)
}

skip_online_tests <- function(msg = "skipping online test") {
  do_online_tests <- getOption("icd.do_online_tests")
  if (is.null(do_online_tests) || !do_online_tests)
    testthat::skip(msg)
}

skip_on_no_rtf <- function(test_year) {
  if (!rtf_year_ok(test_year))
    skip_online_tests(paste(test_year,
                            "ICD-9-CM codes unavailable offline for testsing"))
}

skip_flat_icd9_avail <- function(
  ver = "31",
  msg = paste("skipping test because flat file ICD-9-CM sources not available for version: ", ver)) {
  dat <- icd9_sources[icd9_sources$version == ver, ]
  fn_orig <- dat$short_filename
  if (is.na(fn_orig))
    fn_orig <- dat$other_filename

  # don't try to download the file, just check it is there:
  f_info_short <- unzip_to_data_raw(dat$url,
                                    file_name = fn_orig,
                                    offline = TRUE)
  if (is.null(f_info_short))
    skip_online_tests(msg)

}

skip_icd10cm_flat_avail <- function(msg = "skipping test because flat file ICD-10-CM source not available") {
  if (is.null(icd10cm_get_flat_file(offline = TRUE)))
    skip_online_tests(msg)
}

skip_icd10cm_xml_avail <- function(msg = "skipping test because XML file ICD-10-CM source not available") {
  if (is.null(icd10cm_get_flat_file(offline = TRUE)))
    skip_online_tests(msg)
}

skip_flat_icd9_avail_all <- function() {
  for (v in icd9_sources$version)
    skip_flat_icd9_avail(ver = v)
}

#' expect equal, ignoring any ICD classes
#' @keywords internal
expect_equal_no_icd <- function(object, expected, ..., info = NULL,
                                label = NULL, expected.label = NULL) {
  if (is.null(label))
    label <- find_expr("object")
  if (is.null(expected.label))
    expected.label <- find_expr("expected")

  class(object) <- class(object)[class(object) %nin% icd_all_classes]
  class(expected) <- class(expected)[class(expected) %nin% icd_all_classes]
  eval(bquote(testthat::expect_that(.(object),
                                    testthat::equals(
                                      .(expected), label = expected.label, ...
                                    ), info = info, label = label)))
}

#' expect named sub-chapter has a given range, case insensitive
#' @keywords internal
expect_chap_equal <- function(x, start, end, ver_chaps, ...) {
  x <- tolower(x)
  res <- eval(bquote(testthat::expect_true(.(x) %in% tolower(names(ver_chaps)), ...)))
  if (!res$passed)
    return(res)

  eval(bquote(testthat::expect_equal(.(ver_chaps[[which(tolower(names(ver_chaps)) == x)]]),
                                     c(start = .(start), end = .(end)), ...)))
}

#' @rdname expect_chap_equal
expect_icd10_chap_equal <- function(x, start, end, ...) {
  eval(bquote(expect_chap_equal(.(x), start, end, ver_chaps = icd::icd10_chapters, ...)))
}

#' @rdname expect_chap_equal
expect_icd10_sub_chap_equal <- function(x, start, end, ...) {
  eval(bquote(expect_chap_equal(.(x), .(start), .(end), ver_chaps = icd::icd10_sub_chapters, ...)))
}

#' @rdname expect_chap_equal
expect_icd9_chap_equal <- function(x, start, end, ...) {
  eval(bquote(expect_chap_equal(.(x), start, end, ver_chaps = icd::icd9_chapters, ...)))
}

#' @rdname expect_chap_equal
expect_icd9_sub_chap_equal <- function(x, start, end, ...) {
  eval(bquote(expect_chap_equal(.(x), .(start), .(end), ver_chaps = icd::icd9_sub_chapters, ...)))
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
  eval(bquote(expect_chap_missing(.(x), ver_chaps = icd::icd9_sub_chapters, ...)))
}

expect_icd9_chap_missing <- function(x, ...) {
  eval(bquote(expect_chap_missing(.(x), ver_chaps = icd::icd9_chapters, ...)))
}

expect_icd10_sub_chap_missing <- function(x, ...) {
  eval(bquote(expect_chap_missing(.(x), ver_chaps = icd::icd10_sub_chapters, ...)))
}

expect_icd10_chap_missing <- function(x, ...) {
  eval(bquote(expect_chap_missing(.(x), ver_chaps = icd::icd10_chapters, ...)))
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
  eval(bquote(expect_chap_present(.(x), ver_chaps = icd::icd9_sub_chapters,
                                  info = info, label = label, ...)))
}

#' @rdname expect_chap_present
#' @keywords internal
expect_icd9_chap_present <- function(x, ...) {
  eval(bquote(expect_chap_present(.(x), ver_chaps = icd::icd9_chapters, ...)))
}

#' @rdname expect_chap_present
#' @keywords internal
expect_icd10_sub_chap_present <- function(x, ...) {
  eval(bquote(expect_chap_present(.(x), ver_chaps = icd::icd10_sub_chapters, ...)))
}

#' @rdname expect_chap_present
#' @keywords internal
expect_icd10_chap_present <- function(x, ...) {
  eval(bquote(expect_chap_present(.(x), ver_chaps = icd::icd10_chapters, ...)))
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
#'   testthat::expect_that(icd9("V10"), icd:::icd_classes_are_ordered())
#' @keywords internal
icd_classes_are_ordered <- function() {
  function(x) {
    testthat::expectation(all(icd_classes_ordered(x)),
                          "are not well ordered", "are well ordered")
  }
}

getSlowestTests <- function(n = 5) {
  res <- testthat::test_dir(file.path(".", "tests", "testthat"),
                            reporter = testthat::ListReporter())
  print(tail(res[order(res$real), "test"], n = n))
}

generate_random_pts <- function(...)
  randomOrderedPatients(...)

randomOrderedPatients <- function(...) {
  x <- randomUnorderedPatients(...)
  x[order(x$visitId), ]
}

randomUnorderedPatients <- function(num_patients = 50000, dz_per_patient = 20,
                                    n = num_patients, np = dz_per_patient) {
  set.seed(1441)
  pts <- round(n / np)
  data.frame(
    visitId = sample(seq(1, pts), replace = TRUE, size = n),
    code = c(randomShortIcd9(round(n / 2)), randomShortAhrq(n - round(n / 2))),
    poa = as.factor(
      sample(x = c("Y","N", "n", "n", "y", "X", "E", "", NA),
             replace = TRUE, size = n)),
    stringsAsFactors = FALSE
  )
}

#' generate random short icd9 codes
#' @keywords internal
randomShortIcd9 <- function(n = 50000)
  as.character(floor(stats::runif(min = 1, max = 99999, n = n)))

randomShortAhrq <- function(n = 50000)
  sample(unname(unlist(icd::ahrqComorbid)), size = n, replace = TRUE)

randomDecimalIcd9 <- function(n = 50000)
  paste(
    round(stats::runif(min = 1, max = 999, n = n)),
    sample(icd9ExpandMinor(""), replace = TRUE, size = n),
    sep = "."
  )

#' generate random strings
#'
#' @keywords internal
random_string <- function(n, max_chars = 4) {
  rand_ch <- function()
    sample(c(LETTERS, letters, 0:9, rep("", times = 50)), replace = TRUE, size = n)

  vapply(1:max_chars,
         FUN = function(x) rand_ch(),
         FUN.VALUE = character(n)
  )  %>% apply(1, paste0, collapse = "")
}
