

context("test direct wide data comorbidity calcs")

wide_dat <- data.frame(
  vis = c("1000", "1001"),
  dx1 = c("41003", "39891"),
  # MI, CHF (charlson)
  dx2 = c("0930", "43001"),
  stringsAsFactors = FALSE
) #PVD, Stroke (charlson)
charlson_minimap <- lapply(icd9_map_charlson[1:5], head)

test_that("simple wide data calc", {
  res <- icd:::comorbidMatMulWide(
    wide_dat,
    charlson_minimap,
    id_name = "vis",
    code_names = c("dx1", "dx2")
  )
  expect_equivalent(res["1000",], c(TRUE, FALSE, TRUE, FALSE, FALSE))
  expect_equivalent(res["1001",], c(FALSE, TRUE, FALSE, TRUE, FALSE))
})

test_that("convert long to wide, then do wide cmb", {
  icd9_dfs <-
    c("two_pts",
      "two_pts_fac",
      "ahrq_test_dat",
      "complex_poa_pts",
      "elix_test_dat",
      #"empty_pts",
      "hcc_test_invalid",
      "hcc_test_simple10",
      "hcc_test_simple9",
      "hcc_test_single",
      "multi_comorbid",
      "one_pt_one_icd9",
      "one_pt_two_icd9",
      "pts_invalid_mix",
      "quan_deyo_test_dat",
      "quan_elix_test_dat",
      "random_test_patients",
      "simple_poa_pts",
      "simple_pts",
      "test_twenty"
    )
  for (df in icd9_dfs) {
    expect_error(regexp = NA,
                 by_long <- comorbid_ahrq(get(df)),
                 info = df)
    df_wide <- long_to_wide(get(df))
    icd_names <- names(df_wide)[-1]
    by_wide <- comorbid_ahrq(df_wide, icd_name = icd_names)
    # first, is the data the same?
    expect_equivalent(by_long[order(rownames(by_long)), ], by_wide,
                      info = paste("Wide", df))
    skip("should the order of patients also be the same?")
    expect_equivalent(by_long, by_wide, info = paste("Wide", df))
  }
})
