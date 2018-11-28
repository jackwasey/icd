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

#nocov start

rtf_year_ok <- function(year)
  !is.null(rtf_fetch_year(year, offline = TRUE))

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
  if (is.null(f_info_short)) testthat::skip(msg)
}

skip_flat_icd9_all_avail <- function()
  for (v in icd9_sources$version) skip_flat_icd9_avail(v)

skip_icd10cm_flat_avail <- function(
  msg = "skipping test because flat file ICD-10-CM source not available")
  if (is.null(icd10cm_get_flat_file(offline = TRUE)))
    testthat::skip(msg)

skip_icd10cm_xml_avail <- function(
  msg = "skipping test because XML file ICD-10-CM source not available")
  if (is.null(icd10cm_get_xml_file(offline = TRUE))) testthat::skip(msg)

skip_flat_icd9_avail_all <- function() {
  for (v in icd9_sources$version)
    skip_flat_icd9_avail(ver = v)
}

expect_no_warn <- function(object, expected, ...)
  testthat::expect_warning(object, regexp = NA, ...)

expect_no_error <- function(object, expected, ...)
  testthat::expect_error(object, regexp = NA, ...)

#' expect equal, ignoring any ICD classes
#'
#' Strips any \code{icd} classes (but not others) before making comparison
#' @noRd
#' @keywords internal debugging
expect_equal_no_icd <- function(object, expected, ...) {
  class(object) <- class(object)[class(object) %nin% icd_all_classes]
  class(expected) <- class(expected)[class(expected) %nin% icd_all_classes]
  testthat::expect_equivalent(object, expected, ...)
}

#' @noRd
#' @keywords internal debugging
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
#' @param ... arguments passed to \code{expect_true} in \pkg{testthat} package
#' @keywords internal debugging
expect_chap_equal <- function(x, start, end, ver_chaps, ...) {
  x <- tolower(x)
  res <- eval(bquote(testthat::expect_true(.(x) %in% tolower(names(ver_chaps)), ...)))
  if (!isTRUE(res) || (is.list(res) && !res$passed))
    return(res)
  eval(bquote(testthat::expect_equal(.(ver_chaps[[which(tolower(names(ver_chaps)) == x)]]),
                                     c(start = .(start), end = .(end)), ...)))
}

#' @rdname expect_chap_equal
expect_icd10_sub_chap_equal <- function(x, start, end, ...)
  eval(bquote(expect_chap_equal(.(x), .(start), .(end), ver_chaps = icd10_sub_chapters, ...)))

expect_explain_equal <- function(x, desc, ...)
  eval(bquote(expect_equal(explain_code(.(x)), .(desc), ...)))

#' @rdname expect_chap_equal
expect_icd9_sub_chap_equal <- function(x, start, end, ...) {
  eval(bquote(expect_chap_equal(.(x), .(start), .(end), ver_chaps = icd9_sub_chapters, ...)))
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

expect_chap_missing <- function(x, ver_chaps, info = NULL, label = NULL, ...)
  eval(bquote(expect_true(.(chap_missing(x, ver_chaps)),
                          info = info, label = label, ...)))

expect_icd9_sub_chap_missing <- function(x, ...)
  eval(bquote(expect_chap_missing(.(x), ver_chaps = icd9_sub_chapters, ...)))

expect_icd9_chap_missing <- function(x, ...)
  eval(bquote(expect_chap_missing(.(x), ver_chaps = icd9_chapters, ...)))

expect_icd10_sub_chap_missing <- function(x, ...)
  eval(bquote(expect_chap_missing(.(x), ver_chaps = icd10_sub_chapters, ...)))

expect_icd10_chap_missing <- function(x, ...)
  eval(bquote(expect_chap_missing(.(x), ver_chaps = icd10_chapters, ...)))

#' expect that a chapter with given title exists, case-insensitive
#' @keywords internal debugging
expect_chap_present <- function(x, ver_chaps, info = NULL, label = NULL, ...) {
  eval(bquote(expect_true(.(chap_present(x, ver_chaps = ver_chaps)),
                          info = info, label = label, ...)))
}

#' @rdname expect_chap_present
#' @keywords internal debugging
expect_icd9_sub_chap_present <- function(x, info = NULL, label = NULL, ...) {
  eval(bquote(expect_chap_present(.(x), ver_chaps = icd9_sub_chapters,
                                  info = info, label = label, ...)))
}

#' @rdname expect_chap_present
#' @keywords internal debugging
expect_icd9_chap_present <- function(x, ...) {
  eval(bquote(expect_chap_present(.(x), ver_chaps = icd9_chapters, ...)))
}

#' @rdname expect_chap_present
#' @keywords internal debugging
expect_icd10_sub_chap_present <- function(x, ...) {
  eval(bquote(expect_chap_present(.(x), ver_chaps = icd10_sub_chapters, ...)))
}

#' @rdname expect_chap_present
#' @keywords internal debugging
expect_icd10_chap_present <- function(x, ...) {
  eval(bquote(expect_chap_present(.(x), ver_chaps = icd10_chapters, ...)))
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

#' @describeIn classes_ordered \code{testthat} \code{expect} function
#'   for ICD classes to be in correct order.
#' @keywords internal debugging
expect_classes_ordered <- function(x)
  eval(bquote(testthat::expect_true(classes_ordered(.(x)))))

#' generate random ICD-9 codes
#'
#' @keywords internal debugging datagen
generate_random_short_icd9 <- function(n = 50000)
  as.character(floor(stats::runif(min = 1, max = 99999, n = n)))

#' generate random ICD-9 codes
#'
#' Uses billable ICD-10-CM codes from current master list
#' @param n number to select, passed to \code{sample}
#' @template short_code
#' @keywords internal debugging datagen
generate_random_short_icd10cm_bill <- function(n = 10, short_code = TRUE) {
  x <- sample(unlist(icd10cm2016[icd10cm2016$billable == 1, "code"]),
              replace = TRUE, size = n)
  if (short_code) x else short_to_decimal(x)
}

#' @rdname generate_random_short_icd9
#' @keywords internal debugging datagen
generate_random_decimal_icd9 <- function(n = 50000)
  paste(
    round(stats::runif(min = 1, max = 999, n = n)),
    sample(expand_minor.icd9(""), replace = TRUE, size = n),
    sep = "."
  )

#' @rdname generate_random_short_icd9
#' @keywords internal debugging datagen
generate_random_pts <- function(...)
  generate_random_ordered_pts(...)

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
    visit_id = as_char_no_warn(sample(seq(1, pts), replace = TRUE, size = n)),
    code = fun(n),
    poa = as.factor(
      sample(x = c("Y", "N", "n", "n", "y", "X", "E", "", NA),
             replace = TRUE, size = n)),
    stringsAsFactors = FALSE
  )
}

#' @rdname generate_random_short_icd9
#' @keywords internal debugging datagen
generate_random_short_ahrq_icd9 <- function(n = 50000)
  sample(unname(unlist(icd::icd9_map_ahrq)), size = n, replace = TRUE)

#' generate random strings
#'
#' Mixed upper and lower case, with replacement
#' @keywords internal debugging datagen
random_string <- function(n, max_chars = 4) {
  rand_ch <- function()
    sample(c(LETTERS, letters, 0:9, rep("", times = 50)), replace = TRUE, size = n)

  v <- vapply(1:max_chars,
         FUN = function(x) rand_ch(),
         FUN.VALUE = character(n)
  )
  apply(v, 1, paste0, collapse = "")
}

#' allow \pkg{microbenchmark} to compare multiple results
#' @param x list of values to compare for identity, e.g. results from evaluated
#'   expression in \code{microbenchmark::microbenchmark}
#' @keywords internal
all_identical <- function(x)
  all(sapply(x[-1], function(y) identical(x[[1]], y)))

get_one_of_each <- function()
  c("002.3", "140.25", "245", "285", "290.01", "389.00",
    "390.00", "518", "525", "581", "631", "700", "720", "759.99",
    "765", "780.95", "800", "V02.34", "E900.4")

#' Set up a test environment which also has the internal functions
#' @keywords internal debugging data
test_env <- function() {
  ns <- getNamespace("icd")
  list2env(as.list(ns, all.names = TRUE), parent = parent.env(ns))
}

#' Set system environment to do extra tests
#'
#' These extra tests are not run except for one CI job, and locally as needed.
#' The frequently run tests cover the core code, whereas the extra tests cover
#' things like parsing the ICD and comorbidity definitions.
#' @keywords internal debugging
do_extra_tests <- function(value = "true") {
  Sys.setenv(ICD_TEST_SLOW = value)
  Sys.setenv(ICD_TEST_BUILD_DATA = value)
  Sys.setenv(ICD_TEST_DEPRECATED = value)
}

#' Generate simulated NEDS data for PCCC and wide-data testing
#' @param n Integer number of rows of data to generate
#' @param ncol Integer number of diagnostic code columns, default of 20 matches
#'   NEDS
#' @param icd10 Logical, default `TRUE` to sample ICD-10-CM codes. `FALSE` gives
#'   `ICD-9`
#' @template verbose
#' @examples
#' summary(icd::comorbid_pccc_dx(icd:::generate_neds_pts()))
#' neds <- icd:::generate_neds_pts(n = 100, ncol = 10L, icd10 = FALSE)
#' stopifnot(dim(neds) == c(100L, 11L))
#' summary(icd::comorbid_pccc_dx(neds))
#' \dontrun{
#' # original size data for PCCC benchmarking:
#' set.seed(1441)
#' neds <- icd:::generate_neds_pts(28584301L)
#' neds_comorbid <- icd::comorbid_pccc_dx(neds)
#' }
#' @keywords internal
generate_neds_pts <- function(n = 1000L, ncol = 20L, icd10 = TRUE,
                              verbose = FALSE) {
  codes <- if (icd10) {
    unclass(as_char_no_warn(icd.data::icd10cm2016$code))
  } else {
    unclass(as_char_no_warn(icd.data::icd9cm_hierarchy$code))
  }
  dat <- data.frame(id = as.character(n + seq(n)),
                    icd_code = sample(codes, n, replace = TRUE),
                    stringsAsFactors = TRUE)
  pts_per_code_pos <- as.integer(n / (seq(ncol)) ^ 4)
  dat_wide_factors <- data.frame(id = dat$id,
                                 dx01 = dat$icd_code,
                                 stringsAsFactors = TRUE)
  for (dx in seq(2L, ncol)) {
    dx_str <- sprintf("%02i", dx)
    if (verbose) message("building column:", dx_str)
    len <- pts_per_code_pos[dx]
    l <-  unique(c(NA, sample(codes, len, replace = TRUE)))
    f <- as.integer(sample(c(seq_along(l), rep(1L, n - length(l)))))
    attr(f, "levels") <- l
    attr(f, "class") <- "factor"
    dat_wide_factors[[paste0("dx", dx_str)]] <- f
  }
  dat_wide_str <- dat_wide_factors[1]
  for (i in seq_along(dat_wide_factors)) {
    if (i == 1) next
    dat_wide_str[names(dat_wide_factors)[i]] <-
      as.character(dat_wide_factors[[i]])
  }
  dat_wide_str
}

#nocov end
