context("icd10cm versions")

test_that("active version set to latest version", {
  with_icd10cm_version(
    ver = "2019",
    expect_identical(
      get_icd10cm_active(),
      icd10cm2019
    )
  )
})

test_that("all available data is reported", {
  for (pc in c(TRUE, FALSE)) {
    res <- get_icd10cm_available(pc)
    data_fun_name <- paste0("get_", res)
    expect_true(.exists_in_ns(data_fun_name), info = paste(pc))
  }
})

test_that("temporarily set active version", {
  skip_icd10cm_flat_avail("2014")
  expect_equal(with_icd10cm_version("2014", get_icd10cm_active_ver()), "2014")
  expect_identical(
    object = with_icd10cm_version("2014", nrow(get_icd10cm_active())),
    expected = nrow(get_icd10cm2014()),
    info = paste(
      "With icd10-cm-ver set: ",
      with_icd10cm_version(
        "2014",
        paste(names(.show_options()), .show_options(), sep = "=", collapse = ", ")
      ),
      "Without: ",
      paste(names(.show_options()), .show_options(), sep = "=", collapse = ", ")
    )
  )
})
