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

have_icd_data_resource <- function() {
  !is.null(getOption("icd.data.resource", default = NULL))
}

skip_no_icd_data_resource <- function() {
  if (!have_icd_data_resource()) {
    skip("Skipping - no icd.data.resource option defined.")
  }
}

skip_if_offline <- function() {
  if (.offline()) {
    skip("Offline")
  }
  if (!.interactive()) {
    skip("Not interactive")
  }
}

skip_on_no_rtf <- function(test_year) {
  if (!have_icd_data_resource() || !rtf_year_ok(test_year)) {
    testthat::skip(paste(
      test_year,
      "ICD-9-CM codes unavailable offline for testsing"
    ))
  }
}

skip_flat_icd9_avail <- function(ver, year) {
  skip_no_icd_data_resource()
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
  invisible()
}

skip_flat_icd9_all_avail <- function() {
  for (v in .icd9cm_sources$version) skip_flat_icd9_avail(ver = v)
}

skip_icd10cm_flat_avail <- function(year, dx = TRUE) {
  msg <- "skipping test because flat file ICD-10-CM source not available"
  if (!.exists_in_cache(.get_icd10cm_name(year, dx))) {
    testthat::skip(msg)
  }
  if (.verbose()) message(paste("Not", msg))
  invisible()
}

skip_icd10cm_xml_avail <- function() {
  skip_no_icd_data_resource()
  msg <- "skipping test because XML file ICD-10-CM source not available"
  if (is.null(
    with_absent_action(
      absent_action = "silent",
      with_offline(
        offline = TRUE,
        with_interact(
          interact = FALSE,
          .dl_icd10cm_xml()
        )
      )
    )
  )) {
    testthat::skip(msg)
  }
  invisible()
}

skip_flat_icd9_avail_all <- function() {
  for (v in .icd9cm_sources$version)
    skip_flat_icd9_avail(ver = v)
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

expect_icd10_sub_chap_equal <- function(x, start, end, ...)
  eval(bquote(expect_chap_equal(.(x), .(start), .(end),
    ver_chaps = icd10_sub_chapters,
    ...
  )))

eee <- function(x, desc, ...) {
  x <- sub("\\.", "", x)
  res <- icd9cm_hierarchy[
    icd9cm_hierarchy$code %in% x,
    "long_desc"
  ]
  # eval(bquote(expect_equal(icd::explain_code(.(x)), .(desc), ...)))
  expect_equal(desc, res, ...)
}

eee10 <- function(x, desc, ...) {
  x <- sub("\\.", "", x)
  res <- icd10cm2016[
    icd10cm2016 %in% x,
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

expect_chap_missing <- function(x, ver_chaps, info = NULL, label = NULL, ...)
  eval(bquote(expect_true(.(chap_missing(x, ver_chaps)),
    info = info, label = label, ...
  )))

expect_icd9_sub_chap_missing <- function(x, ...)
  eval(bquote(expect_chap_missing(.(x), ver_chaps = icd9_sub_chapters, ...)))

expect_icd9_chap_missing <- function(x, ...)
  eval(bquote(expect_chap_missing(.(x), ver_chaps = icd9_chapters, ...)))

expect_icd10_sub_chap_missing <- function(x, ...)
  eval(bquote(expect_chap_missing(.(x), ver_chaps = icd10_sub_chapters, ...)))

expect_icd10_chap_missing <- function(x, ...)
  eval(bquote(expect_chap_missing(.(x), ver_chaps = icd10_chapters, ...)))

# expect that a chapter with given title exists, case-insensitive
expect_chap_present <- function(x, ver_chaps, info = NULL, label = NULL, ...) {
  eval(bquote(expect_true(.(chap_present(x, ver_chaps = ver_chaps)),
    info = info, label = label, ...
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
skip_missing_icd10who <- function(ver = "2016", lang = "en") {
  if (ver == "2016" && lang == "en") {
    if (is.null(.get_icd10who2016(must_work = FALSE))) {
      testthat::skip("English WHO ICD-10 not available")
    }
  } else if (ver == "2008" && lang == "fr") {
    if (is.null(.get_icd10who2008fr(must_work = FALSE))) {
      testthat::skip("French WHO ICD-10 not available")
    }
  } else {
    stop("Unavailable year/language combination for WHO codes sought.")
  }
}

skip_missing_dat <- function(var_name) {
  if (!.exists_in_cache(var_name)) {
    skip(paste(var_name, "not available"))
  }
}
