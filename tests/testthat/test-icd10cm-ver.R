context("icd10cm versions")

test_that("active version set to latest version", {
  with_icd10cm_version(
    ver = "2019",
    code = expect_identical(
      get_icd10cm_active(),
      icd10cm2019
    )
  )
  # and the other way, because something funny is going on during testing only
  expect_identical(
    with_icd10cm_version(
      ver = "2019",
      code = get_icd10cm_active()
    ),
    icd10cm2019
  )
  # and for good measure
  skip_missing_dat("icd10cm2019")
  expect_identical(
    get_icd10cm2019(),
    icd10cm2019
  )
  skip_missing_dat("icd10cm2017")
  expect_identical(
    icd::get_icd10cm2017(),
    icd:::.get_icd10cm2017(must_work = TRUE)
  )
})

test_that("all available ICD-10-CM data is reported and gettable", {
  for (pc in c(TRUE, FALSE)) {
    # this gets all possible data available, not what is actually cached
    res <- get_icd10cm_available(dx = !pc)
    cache_getter_names <- .get_getter_name(res)
    fetcher_name <- .get_fetcher_name(res)
    expect_true(.exists_in_ns(cache_getter_names),
      info = paste("procedure code =", pc)
    )
    skip_if_offline()
    skip_on_appveyor()
    skip_on_travis()
    skip_on_cran()
    for (r in res) {
      expect_error(
        regexp = NA,
        .get_fetcher_fun(r)(),
        info = paste("Running fetcher for: r =", r, "and pc =", pc)
      )
      cache_getter_name <- .get_getter_name(r)
      expect_is(
        object = do.call(cache_getter_name, args = list()),
        class = "data.frame",
        info = paste("Calling cache getter :", cache_getter_name, "for: r =", r, "and pc =", pc)
      )
    }
    expect_true(
      all(
        .exists_anywhere(res)
      ),
      info = paste("procedure code =", pc)
    )
  }
})

test_that("temporarily set active version", {
  skip_icd10cm_flat_avail("2014")
  expect_equal(
    with_icd10cm_version(
      ver = "2014",
      code = {
        get_icd10cm_active_year()
      }
    ),
    "2014"
  )
  expect_identical(
    object = with_icd10cm_version("2014", {
      writeLines(paste(as.character(icd:::.show_options()), collapse = ", "),
        con = "~/icddebug.txt"
      )
      nrow(get_icd10cm_active())
    }),
    expected = nrow(get_icd10cm2014()),
    info = paste(
      "With icd10-cm-ver set: ",
      with_icd10cm_version(
        ver = "2014",
        code = {
          if (testthat::is_testing()) {
            debugtxt <- paste(names(icd:::.show_options()),
              as.character(icd:::.show_options()),
              sep = "=",
              collapse = ", \n"
            )
            # writeLines(debugtxt, con = "~/icddebug.txt")
            message(debugtxt)
          }
          paste(names(.show_options()), .show_options(), sep = "=", collapse = ", ")
        }
      ),
      "Without: ",
      paste(names(.show_options()), .show_options(), sep = "=", collapse = ", ")
    )
  )
})
