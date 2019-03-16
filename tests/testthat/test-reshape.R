context("reshaping wide to long")
longdf <- data.frame(
  visit_id = c("a", "b", "b", "c"),
  icd9 = c("441", "4424", "443", "441")
)
widedf <- data.frame(
  visit_id = c("a", "b", "c"),
  icd9_001 = c("441", "4424", "441"),
  icd9_002 = c(NA, "443", NA)
)
test_that("long data to wide data", {
  longcmp <- data.frame(
    visit_id = c("a", "b", "c"),
    icd_001 = c("441", "4424", "441"),
    icd_002 = c(NA, "443", NA)
  )
  expect_equivalent(res <- long_to_wide(longdf), longcmp)
  expect_true(is.icd_wide_data(res))
  longcmp2 <- data.frame(
    visit_id = c("a", "b", "c"),
    icd_001 = c("441", "4424", "441"),
    icd_002 = c(NA, "443", NA),
    icd_003 = c(NA, NA, NA)
  )
  expect_equivalent(res <- long_to_wide(longdf, min_width = 3), longcmp2)
  expect_true(is.icd_wide_data(res))
  longdf2 <- data.frame(
    i = c("441", "4424", "443", "441"),
    v = c("a", "b", "b", "c")
  )
  expect_equal(
    names(long_to_wide(longdf2,
      visit_name = "v",
      icd_name = "i",
      prefix = "ICD10_"
    )),
    c("v", "ICD10_001", "ICD10_002")
  )
})

test_that("wide data to long data", {
  expect_equivalent(
    wide_to_long(widedf),
    longdf
  )
  # same with "" instead of NA.
  widedfempty <- data.frame(
    visit_id = c("a", "b", "c"),
    icd9_001 = c("441", "4424", "441"),
    icd9_002 = c("", "443", "")
  )
  expect_equivalent(
    wide_to_long(widedfempty),
    longdf
  )
})

test_that("matrix to data frame and back", {
  for (pts in list(random_test_patients, test_twenty, multi_comorbid)) {
    mat <- comorbid_ahrq(pts)
    df <- comorbid_mat_to_df(mat)
    expect_identical(comorbid_df_to_mat(df), mat)
  }
})

test_that("dataframe to matrix and back", {
  for (pts in list(random_test_patients, test_twenty, multi_comorbid)) {
    df2 <- comorbid_ahrq(pts, return_df = TRUE)
    mat2 <- comorbid_df_to_mat(df2)
    df3 <- comorbid_mat_to_df(mat2,
      visit_name = "visit_id",
      stringsAsFactors = FALSE
    )
    expect_identical(df2, df3)
  }
})
