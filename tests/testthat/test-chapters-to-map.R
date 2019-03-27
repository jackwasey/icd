context("slower tests")

test_that("chapters to map", {
  # Slow for huge chapters, like II (Cancer)
  skip_if_not_installed("icd.data")
  n <- 3:6
  codes <- icd:::get_one_of_each()[n]
  ooe <- icd_long_data(
    visit_id = sprintf("pt%02d", n),
    code = codes,
    stringsAsFactors = TRUE
  )
  class(ooe[["code"]]) <- c("icd9", "icd_decimal_diag", "factor")
  expect_warning(
    test_map <- chapters_to_map(
      icd.data::icd9_chapters[n],
      defined = FALSE
    ),
    regexp = NA
  )
  expect_warning(
    cmb <- icd9_comorbid(
      x = ooe,
      short_code = FALSE,
      map = test_map,
      short_map = TRUE,
      return_df = TRUE
    ),
    regexp = NA
  )
  cmbcmp <- unname(as.matrix(icd:::logical_to_binary(cmb)[-1]))
  expmat <- diag(nrow = length(ooe$code))
  expect_equivalent(cmbcmp, expmat)
})
