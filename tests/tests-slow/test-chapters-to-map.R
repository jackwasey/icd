context("slow tests")

test_that("convert icd-9 ranges", {
  ooe <- icd_long_data(
    visit_id = sprintf("pt%02d", seq_along(icd:::get_one_of_each())),
    code = icd:::get_one_of_each(),
    stringsAsFactors = TRUE)
  class(ooe[["code"]]) <- c("icd9", "icd_decimal_diag", "factor")
  expect_warning(
    test_map <- icd:::icd9_chapters_to_map(icd9_chapters), regexp = NA)
  expect_warning(
    cmb <- icd9_comorbid(x = ooe, short_code = FALSE, map = test_map,
                         short_map = TRUE, return_df = TRUE), regexp = NA)
  cmbcmp <- unname(as.matrix(icd:::logical_to_binary(cmb)[-1]))
  expmat <- diag(nrow = length(ooe$code))
  expect_equivalent(cmbcmp, expmat)
})
