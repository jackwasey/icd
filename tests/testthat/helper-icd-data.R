# !diagnostics suppress=year
# (apparently non-functional attempt to suppress incorrect rstudio diagnostic)


skip_no_icd_data_raw <- function(
                                 fun,
                                 msg = paste0(fun, "() did not find existing raw data.")) {
  if (with_offline(
    TRUE,
    with_absent_action(
      "silent",
      with_interact(FALSE, is.null(fun()))
    )
  )) {
    skip(msg)
  }
}

rtf_year_ok <- function(year, ...) {
  !is.null(
    with_absent_action(
      absent_action = "silent",
      with_offline(
        offline = TRUE,
        with_interact(
          interact = FALSE,
          .dl_icd9cm_rtf_year(year, ...)
        )
      )
    )
  )
}

have_cache <- function() {
  opt_ok <- !is.null(.get_opt("cache", default = NULL))
  if (!opt_ok && dir.exists(.default_icd_data_dir())) {
    if (.verbose()) {
      message("have_cache() is setting data opt to default")
    }
    .set_opt("cache" = .default_icd_data_dir())
    return(TRUE)
  }
  opt_ok
}

skip_no_icd_data_cache <- function() {
  if (!have_cache()) {
    skip("Skipping - no cache option defined.")
  }
}

skip_if_offline <- function() {
  if (.offline()) {
    skip("Offline")
  }
}

skip_on_no_rtf <- function(test_year) {
  if (!have_cache() || !rtf_year_ok(test_year)) {
    testthat::skip(paste(
      test_year,
      "ICD-9-CM codes unavailable offline for testsing"
    ))
  }
}

skip_flat_icd9_avail <- function(ver = NULL, year = NULL) {
  skip_no_icd_data_cache()
  if (missing(ver)) {
    if (missing(year)) stop("specify ver or year")
    ver <- .icd9cm_sources[.icd9cm_sources$year == year, "version"]
  } else {
    year <- .icd9cm_sources[.icd9cm_sources$version == ver, "f_year"]
  }
  msg <- paste(
    "skipping test because flat file ICD-9-CM",
    "sources not available for version: ", ver, ", year: ", year
  )
  with_absent_action(absent_action = "silent", {
    dat <- .get_fetcher_fun(paste0("icd9cm", year, "_leaf"))()
    if (is.null(dat)) testthat::skip(msg)
  })
  invisible(NULL)
}

skip_flat_icd9_all_avail <- function() {
  for (v in .icd9cm_sources$version) skip_flat_icd9_avail(ver = v)
}

skip_icd10cm_flat_avail <- function(year, dx = TRUE) {
  msg <- paste(
    "skipping test flat file ICD-10-CM source for",
    year, ", and dx = ", dx
  )
  if (!.exists_in_cache(.get_icd10cm_name(year, dx))) {
    testthat::skip(msg)
  }
  .msg(paste("Not", msg))
  invisible(NULL)
}

skip_icd10cm_xml_avail <- function() {
  skip_no_icd_data_cache()
  skip_no_icd_data_raw(
    .dl_icd10cm_xml,
    "skipping test because XML file ICD-10-CM source not available"
  )
}

skip_flat_icd9_avail_all <- function() {
  for (v in .icd9cm_sources$version) {
    skip_flat_icd9_avail(ver = v)
  }
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
#' @noRd
expect_chap_equal <- function(x, start, end, ver_chaps, ...) {
  x <- tolower(x)
  res <- eval(bquote(
    testthat::expect_true(.(x) %in% tolower(names(ver_chaps)), ...)
  ))
  if (!isTRUE(res) || (is.list(res) && !res$passed)) {
    return(res)
  }
  eval(bquote(testthat::expect_equal(
    .(ver_chaps[[which(tolower(names(ver_chaps)) == x)]]),
    c(start = .(start), end = .(end)), ...
  )))
}

expect_icd10_sub_chap_equal <- function(x, start, end, ...) {
  eval(bquote(expect_chap_equal(.(x), .(start), .(end),
    ver_chaps = icd10_sub_chapters,
    ...
  )))
}

eee <- function(x, desc, ...) {
  x <- sub("\\.", "", x)
  res <- icd9cm_hierarchy[
    icd9cm_hierarchy$code %in% x,
    "long_desc"
  ]
  expect_equal(desc, res, ...)
}

eee10 <- function(x, desc, ...) {
  x <- sub("\\.", "", x)
  res <- icd10cm2019[
    icd10cm2019 %in% x,
    "long_desc"
  ]
  if (nrow(res) == 0 && is.null(x)) {
    expect_true(TRUE)
  } else {
    expect_equal(desc, res, ...)
  }
}

expect_icd9_sub_chap_equal <- function(x, start, end, ...) {
  eval(bquote(expect_chap_equal(.(x), .(start), .(end),
    ver_chaps = icd9_sub_chapters,
    ...
  )))
}

expect_icd9_major_equals <- function(x, code, ...) {
  res <- eval(bquote(expect_true(.(x) %in% names(icd9_majors), ...)))
  if (isTRUE(res) || (is.list(res) && res$passed)) {
    eval(bquote(expect_equal(icd9_majors[[.(x)]], .(code), ...)))
  } else {
    res
  }
}

expect_icd9_major_is_sub_chap <- function(x, code, ...) {
  res <- eval(bquote(expect_icd9_sub_chap_equal(.(x),
    start = .(code),
    end = .(code), ...
  )))
  if (isTRUE(res) || (is.list(res) && res$passed)) {
    eval(bquote(expect_icd9_major_equals(.(x), .(code), ...)))
  } else {
    res
  }
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
  eval(
    bquote(
      expect_true(
        object = .(chap_missing(x, ver_chaps)),
        info = info,
        label = label
      )
    )
  )
}

expect_icd9_sub_chap_missing <- function(x, ...) {
  eval(bquote(expect_chap_missing(.(x), ver_chaps = icd9_sub_chapters, ...)))
}

expect_icd9_chap_missing <- function(x, ...) {
  eval(bquote(expect_chap_missing(.(x), ver_chaps = icd9_chapters, ...)))
}

expect_icd10_sub_chap_missing <- function(x, ...) {
  eval(bquote(expect_chap_missing(.(x), ver_chaps = icd10_sub_chapters, ...)))
}

expect_icd10_chap_missing <- function(x, ...) {
  eval(bquote(expect_chap_missing(.(x), ver_chaps = icd10_chapters, ...)))
}

# expect that a chapter with given title exists, case-insensitive
expect_chap_present <- function(x, ver_chaps, info = NULL, label = NULL, ...) {
  eval(bquote(expect_true(.(chap_present(x, ver_chaps = ver_chaps)),
    info = info, label = label
  )))
}

expect_icd9_sub_chap_present <- function(x, info = NULL, label = NULL, ...) {
  eval(bquote(expect_chap_present(.(x),
    ver_chaps = icd9_sub_chapters,
    info = info, label = label, ...
  )))
}

expect_icd9_chap_present <- function(x, ...) {
  eval(bquote(expect_chap_present(.(x), ver_chaps = icd9_chapters, ...)))
}

expect_icd10_sub_chap_present <- function(x, ...) {
  eval(bquote(expect_chap_present(.(x), ver_chaps = icd10_sub_chapters, ...)))
}

expect_icd10_chap_present <- function(x, ...) {
  eval(bquote(expect_chap_present(.(x), ver_chaps = icd10_chapters, ...)))
}

expect_icd9_only_chap <- function(x, ...) {
  res <- eval(bquote(expect_icd9_chap_present(.(x), ...)))
  if (isTRUE(res) || (is.list(res) && res$passed)) {
    eval(bquote(expect_icd9_sub_chap_missing(.(x), ...)))
  } else {
    res
  }
}

expect_icd9_only_sub_chap <- function(x, info = NULL, label = NULL, ...) {
  res <- eval(bquote(expect_icd9_sub_chap_present(.(x),
    info = info,
    label = label,
    ...
  )))
  if (isTRUE(res) || (is.list(res) && res$passed)) {
    eval(bquote(expect_icd9_chap_missing(.(x),
      info = info,
      label = label,
      ...
    )))
  } else {
    res
  }
}

expect_icd10_only_chap <- function(x, ...) {
  res <- eval(bquote(expect_icd10_chap_present(.(x), ...)))
  if (isTRUE(res) || (is.list(res) && res$passed)) {
    eval(bquote(expect_icd10_sub_chap_missing(.(x), ...)))
  } else {
    res
  }
}

expect_icd10_only_sub_chap <- function(x, ...) {
  res <- eval(bquote(expect_icd10_sub_chap_present(.(x), ...)))
  if (isTRUE(res) || (is.list(res) && res$passed)) {
    eval(bquote(expect_icd10_chap_missing(.(x), ...)))
  } else {
    res
  }
}

#' expect equal, ignoring any ICD classes
#'
#' Strips any \code{icd} classes (but not others) before making comparison
#' @noRd
#' @keywords internal debugging
expect_equal_no_icd <- function(object, expected, ...) {
  # taken from icd class.R
  icd_all_classes <- c(
    "icd9cm",
    "icd9",
    "icd10cm",
    "icd10who",
    "icd10",
    "icd_long_data",
    "icd_wide_data",
    "comorbidity_map"
  )
  class(object) <- class(object)[class(object) %nin% icd_all_classes]
  class(expected) <- class(expected)[class(expected) %nin% icd_all_classes]
  testthat::expect_equivalent(object, expected, ...)
}

#' Skip subsequent tests that depend on WHO ICD-10 data if not available
#'
#' Will only skip if the data is absent. For use by 'icd' package, and will be
#' removed in the future.
#' @param ver Version of WHO ICD-10 to use, currently a four-digit year
#' @param lang Language, currently either 'en' or 'fr'
#' @noRd
skip_missing_icd10who <- function(ver = "2016", lang = "en") {
  if (ver == "2016" && lang == "en") {
    if (!.exists_in_cache("icd10who2016")) {
      testthat::skip("English WHO ICD-10 not available")
    }
  } else if (ver == "2008" && lang == "fr") {
    if (!.exists_in_cache("icd10who2008fr")) {
      testthat::skip("French WHO ICD-10 not available")
    }
  } else {
    stop("Unavailable year/language combination for WHO codes sought.")
  }
}

skip_missing_dat <- function(var_name) {
  if (!.exists_in_cache(var_name, USE.NAMES = FALSE)) {
    skip(paste(var_name, "not available"))
  }
}

skip_missing_icd10fr <- function() {
  if (!.exists_anywhere("icd10fr2019")) {
    testthat::skip("No ICD-10-FR 2019 French data")
  }
}

skip_slow <- function(msg = "Skipping slow test") {
  testthat::skip_on_cran()
  if (!.get_opt("test_slow", default = FALSE)) {
    testthat::skip(msg)
  }
}

skip_no_icd_data_raw <- function(
                                 fun,
                                 msg = paste0(fun, "() did not find existing raw data.")) {
  if (with_offline(
    TRUE,
    with_absent_action(
      "silent",
      is.null(fun())
    )
  )) {
    skip(msg)
  }
}
