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

do_slow_tests <- function(x = TRUE) {
  options("icd.do_slow_tests" = x)
}

doing_slow_tests <- function() {
  identical(getOption("icd.do_slow_tests"), FALSE)
}

skip_slow_tests <- function(msg = "skipping slow test") {
  if (doing_slow_tests())
    testthat::skip(msg)
}

rtf_year_ok <- function(year) {
  !is.null(fetch_rtf_year(year, offline = TRUE))
}

skip_on_no_rtf <- function(test_year) {
  if (!rtf_year_ok(test_year))
    testthat::skip(paste(test_year,
                         "ICD-9-CM codes unavailable offline for testsing"))
}

skip_flat_icd9_avail <- function(ver = "31") {
  msg <- paste("skipping test because flat file ICD-9-CM",
               "sources not available for version: ", ver)
  dat <- icd9_sources[icd9_sources$version == ver, ]
  fn_orig <- dat$short_filename
  if (is.na(fn_orig))
    fn_orig <- dat$other_filename

  # don't try to download the file, just check it is there:
  f_info_short <- unzip_to_data_raw(dat$url,
                                    file_name = fn_orig,
                                    offline = TRUE)
  if (is.null(f_info_short))
    testthat::skip(msg)
}

skip_flat_icd9_all_avail <- function() {
  for (v in icd9_sources$version)
    skip_flat_icd9_avail(v)
}

skip_icd10cm_flat_avail <- function(msg = "skipping test because flat file ICD-10-CM source not available") {
  if (is.null(icd10cm_get_flat_file(offline = TRUE)))
    testthat::skip(msg)
}

skip_icd10cm_xml_avail <- function(msg = "skipping test because XML file ICD-10-CM source not available") {
  if (is.null(icd10cm_get_xml_file(offline = TRUE)))
    testthat::skip(msg)
}

skip_flat_icd9_avail_all <- function() {
  for (v in icd9_sources$version)
    skip_flat_icd9_avail(ver = v)
}

#' expect equal, ignoring any ICD classes
#'
#' Strips any \code{icd} classes (but not others) before making comparison
#' @keywords internal debugging
expect_equal_no_icd <- function(object, expected, ...) {
  class(object) <- class(object)[class(object) %nin% icd_all_classes]
  class(expected) <- class(expected)[class(expected) %nin% icd_all_classes]
  eval(bquote(testthat::expect_equivalent(.(object), .(expected), ...)))
}

expect_equal_no_class_order <- function(object, expected, ...) {
  eval(bquote(testthat::expect_true(all(class(.(object)) %in% class(.(expected))), ...)))
  eval(bquote(testthat::expect_equivalent(unclass(.(object)), unclass(.(expected)), ...)))
}

#' expect named sub-chapter has a given range, case insensitive
#'
#' First checks that the given name is indeed in \code{ver_chaps},
#' and if so, checks whether that entry in \code{ver_chaps} matches
#' the given \code{start} and \code{end}.
#' @param x name of a chapter
#' @param start ICD code
#' @param end ICD code
#' @param ver_chaps list with each member being a start-end pair, and names
#'   being the chapter names
#' @param ... arguments passed to \code{\link[testthat]{expect_true}}
#' @keywords internal debugging
expect_chap_equal <- function(x, start, end, ver_chaps, ...) {
  x <- tolower(x)
  res <- eval(bquote(testthat::expect_true(.(x) %in% tolower(names(ver_chaps)), ...)))
  # new testthat doesn't return a result object, but TRUE
  if (!isTRUE(res) || (is.list(res) && !res$passed))
    return(res)

  eval(bquote(testthat::expect_equal(.(ver_chaps[[which(tolower(names(ver_chaps)) == x)]]),
                                     c(start = .(start), end = .(end)), ...)))
}

#' @rdname expect_chap_equal
expect_icd10_sub_chap_equal <- function(x, start, end, ...) {
  eval(bquote(expect_chap_equal(.(x), .(start), .(end), ver_chaps = icd::icd10_sub_chapters, ...)))
}

#' @rdname expect_chap_equal
expect_icd9_sub_chap_equal <- function(x, start, end, ...) {
  eval(bquote(expect_chap_equal(.(x), .(start), .(end), ver_chaps = icd::icd9_sub_chapters, ...)))
}

expect_icd9_major_equals <- function(x, code, ...) {
  res <- eval(bquote(expect_true(.(x) %in% names(icd9_majors), ...)))
  if (isTRUE(res) || (is.list(res) && res$passed))
    eval(bquote(expect_equal(icd9_majors[[.(x)]], .(code), ...)))
  else
    res
}

expect_icd9_major_is_sub_chap <- function(x, code, ...) {
  res <- eval(bquote(expect_icd9_sub_chap_equal(.(x), start = .(code), end = .(code), ...)))
  if (isTRUE(res) || (is.list(res) && res$passed))
    eval(bquote(expect_icd9_major_equals(.(x), .(code), ...)))
  else
    res
}

chap_missing <- function(x, ver_chaps, ...) {
  x <- tolower(x)
  lnames <- tolower(names(ver_chaps))
  x %nin% lnames
}

chap_present <- function(x, ver_chaps, ...) {
  x <- tolower(x)
  lnames <- tolower(names(ver_chaps))
  x %in% lnames
}

expect_chap_missing <- function(x, ver_chaps, info = NULL, label = NULL, ...) {
  eval(bquote(expect_true(.(chap_missing(x, ver_chaps)),
                          info = info, label = label, ...)))
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
#' @keywords internal debugging
expect_chap_present <- function(x, ver_chaps, info = NULL, label = NULL, ...) {
  eval(bquote(expect_true(.(chap_present(x, ver_chaps = ver_chaps)),
                          info = info, label = label, ...)))
}

#' @rdname expect_chap_present
#' @keywords internal debugging
expect_icd9_sub_chap_present <- function(x, info = NULL, label = NULL, ...) {
  eval(bquote(expect_chap_present(.(x), ver_chaps = icd::icd9_sub_chapters,
                                  info = info, label = label, ...)))
}

#' @rdname expect_chap_present
#' @keywords internal debugging
expect_icd9_chap_present <- function(x, ...) {
  eval(bquote(expect_chap_present(.(x), ver_chaps = icd::icd9_chapters, ...)))
}

#' @rdname expect_chap_present
#' @keywords internal debugging
expect_icd10_sub_chap_present <- function(x, ...) {
  eval(bquote(expect_chap_present(.(x), ver_chaps = icd::icd10_sub_chapters, ...)))
}

#' @rdname expect_chap_present
#' @keywords internal debugging
expect_icd10_chap_present <- function(x, ...) {
  eval(bquote(expect_chap_present(.(x), ver_chaps = icd::icd10_chapters, ...)))
}

expect_icd9_only_chap <- function(x, ...) {
  res <- eval(bquote(expect_icd9_chap_present(.(x), ...)))
  if (isTRUE(res) || (is.list(res) && res$passed))
    eval(bquote(expect_icd9_sub_chap_missing(.(x), ...)))
  else
    res
}

expect_icd9_only_sub_chap <- function(x, info = NULL, label = NULL, ...) {
  res <- eval(bquote(expect_icd9_sub_chap_present(.(x), info = info, label = label, ...)))
  if (isTRUE(res) || (is.list(res) && res$passed))
    eval(bquote(expect_icd9_chap_missing(.(x), info = info, label = label, ...)))
  else
    res
}

expect_icd10_only_chap <- function(x, ...) {
  res <- eval(bquote(expect_icd10_chap_present(.(x), ...)))
  if (isTRUE(res) || (is.list(res) && res$passed))
    eval(bquote(expect_icd10_sub_chap_missing(.(x), ...)))
  else
    res
}

expect_icd10_only_sub_chap <- function(x, ...) {
  res <- eval(bquote(expect_icd10_sub_chap_present(.(x), ...)))
  if (isTRUE(res) || (is.list(res) && res$passed))
    eval(bquote(expect_icd10_chap_missing(.(x), ...)))
  else
    res

}

#' @describeIn icd_classes_ordered \code{testthat} \code{expect} function
#'   for ICD classes to be in correct order.
#' @keywords internal debugging
expect_icd_classes_ordered <- function(x) {
  eval(bquote(testthat::expect_true(icd_classes_ordered(.(x)))))
}

#' generate random ICD-9 codes
#'
#' @keywords internal debugging datagen
generate_random_short_icd9 <- function(n = 50000) {
  as.character(floor(stats::runif(min = 1, max = 99999, n = n)))
}

#' generate random ICD-9 codes
#'
#' Uses billable ICD-10-CM codes from current master list
#' @param n number to select, passed to \code{sample}
#' @template short_code
#' @keywords internal debugging datagen
generate_random_short_icd10cm_bill <- function(n = 10, short_code = TRUE) {
  x <- sample(unlist(icd10cm2016[icd10cm2016$billable == 1, "code"]), replace = TRUE, size = n)
  if (short_code)
    x
  else
    icd_short_to_decimal(x)
}

#' @rdname generate_random_short_icd9
#' @keywords internal debugging datagen
generate_random_decimal_icd9 <- function(n = 50000)
  paste(
    round(stats::runif(min = 1, max = 999, n = n)),
    sample(icd_expand_minor.icd9(""), replace = TRUE, size = n),
    sep = "."
  )

#' @rdname generate_random_short_icd9
#' @keywords internal debugging datagen
generate_random_pts <- function(...) {
  generate_random_ordered_pts(...)
}

#' @rdname generate_random_short_icd9
#' @keywords internal debugging datagen
generate_random_ordered_pts <- function(...) {
  x <- generate_random_unordered_pts(...)
  x[order(x$visit_id), ]
}

#' @rdname generate_random_short_icd9
#' @keywords internal debugging datagen
generate_random_unordered_pts <- function(num_patients = 50000, dz_per_patient = 20,
                                          n = num_patients, np = dz_per_patient,
                                          fun = generate_random_short_icd9) {
  set.seed(1441)
  pts <- round(n / np)
  data.frame(
    visit_id = sample(seq(1, pts), replace = TRUE, size = n),
    code = fun(n),
    poa = as.factor(
      sample(x = c("Y", "N", "n", "n", "y", "X", "E", "", NA),
             replace = TRUE, size = n)),
    stringsAsFactors = FALSE
  )
}

#' @rdname generate_random_short_icd9
#' @keywords internal debugging datagen
generate_random_short_ahrq_icd9 <- function(n = 50000) {
  sample(unname(unlist(icd::icd9_map_ahrq)), size = n, replace = TRUE)
}

#' generate random strings
#'
#' Mixed upper and lower case, with replacement
#' @keywords internal debugging datagen
random_string <- function(n, max_chars = 4) {
  rand_ch <- function()
    sample(c(LETTERS, letters, 0:9, rep("", times = 50)), replace = TRUE, size = n)

  vapply(1:max_chars,
         FUN = function(x) rand_ch(),
         FUN.VALUE = character(n)
  )  %>% apply(1, paste0, collapse = "")
}

#' Show options which control testing
#'
#' Get the options for all currently used \code{icd} testing options.
#' @keywords internal debugging
show_test_options <- function() {
  print(options("icd.do_slow_tests"))
  print(options("icd.warn_deprecated"))
}

# nocov start

#' Set test options to do everything
#'
#' Default without setting options is for slow and online tests to be skipped,
#' and warnings to be generated for deprecated functions. This function
#' explicitly sets options to do slow and online tests, and not to warn for
#' deprecated functions. This is intended for local testing.
#' @keywords internal debugging
set_full_test_options <- function() {

  message("current test options:")
  show_test_options()

  message("now setting full test options")
  options("icd.do_slow_tests" = TRUE)
  options("icd.warn_deprecated" = FALSE)
  show_test_options()
}
# nocov end

#' Set-up test options
#'
#' Checks shell environment for whether to do slow or online tests.
#' Checks whether we should warn for deprecated functions (usually not
#' when testing). If \code{covr} is detected (an option is set), then
#' we may be in a sub-process and not see any shell environment or options
#' from the calling process, so try to set slow tests on and warnings off.
#' @keywords internal debugging
setup_test_check <- function() {

  # basic defaults if nothing else is set: skip slow and online tests and warn
  # deprecated
  if (is.null(options("icd.do_slow_tests")))
    options("icd.do_slow_tests" = FALSE)
  if (is.null(options("icd.warn_deprecated")))
    options("icd.warn_deprecated" = TRUE)

  # covr runs tests in a completely different R process, so seem like options are not
  # preserved.. An alternative might be to add an expression to be run to covr
  # package_coverage command (or caller).
  if (!is.null(getOption("covr.exclude_pattern"))) {
    message("covr detected so doing slow tests")
    show_test_options()
    if (!is.null(getOption("icd.do_slow_tests")))
      options("icd.do_slow_tests" = TRUE)
    options("icd.warn_deprecated" = FALSE)
    show_test_options()

    # also try to turn off all other warnings, e.g. testthat 'info' deprecation.
    options("warn" = -1)
  }

  if (identical(tolower(Sys.getenv("ICD_SLOW_TESTS")), "true")) {
    message("environment variable ICD_SLOW_TESTS found to be true, so doing slow tests")
    options("icd.do_slow_tests" = TRUE)
  }
  # nocov start
  if (identical(tolower(Sys.getenv("ICD_WARN_DEPRECATED")), "true")) {
    message("environment variable ICD_WARN_DEPRECATE found to be true, so warning for deprecated icd9 function use")
    options("icd.warn_deprecated" = TRUE)
  }
  # nocov end
}

#' run \code{testtthat::test_check} with a Perl regular expression filter
#'
#' Use 'summary' reporter so that \code{covr} produces output and doesn't time-out on
#' Travis. The code coverage testing is slower than regular testing because of
#' instrumentation.
#' @param pattern PERL regular expression to match tests
#' @param msg character, if given will give this message, otherwise,
#'   messages the regular expression
#' @keywords internal debugging
my_test_check <- function(pattern, msg) {
  if (missing(msg))
    msg <- pattern

  message(msg)
  # use Perl for grepl to interpret the regex which can then include negative
  # backrefs to exclude things.
  testthat::test_check("icd", filter = pattern, perl = TRUE, reporter = "summary")
}
