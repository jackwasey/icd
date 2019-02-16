context("slower tests")

test_that("chapters to map", {
  skip_slow("takes quite a few seconds, and low yield")
  ooe <- icd_long_data(
    visit_id = sprintf("pt%02d", seq_along(icd:::get_one_of_each())),
    code = icd:::get_one_of_each(),
    stringsAsFactors = TRUE)
  class(ooe[["code"]]) <- c("icd9", "icd_decimal_diag", "factor")
  expect_warning(
    test_map <- icd:::icd9_chapters_to_map(
      icd.data::icd9_chapters,
      defined = FALSE),
    regexp = NA)
  expect_warning(
    cmb <- icd9_comorbid(x = ooe, short_code = FALSE, map = test_map,
                         short_map = TRUE, return_df = TRUE),
    regexp = NA)
  cmbcmp <- unname(as.matrix(icd:::logical_to_binary(cmb)[-1]))
  expmat <- diag(nrow = length(ooe$code))
  expect_equivalent(cmbcmp, expmat)
})
