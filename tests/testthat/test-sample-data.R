context("sample data")
test_that("ICD-10 codes in uranium data are okay", {
  expect_true(all(icd::is_valid(uranium_pathology$icd10, short_code = FALSE)))
  skip("not sure whether there are errors in this data yet.")
  expect_equal(
    setdiff(
      levels(icd::decimal_to_short(uranium_pathology$icd10)),
      icd10cm2016$code
    ),
    character(0)
  )
})

test_that("sample data frames have correct class", {
  expect_true(icd::is.icd_long_data(uranium_pathology))
  expect_true(icd::is.icd_wide_data(vermont_dx))
  # should not have vector classes on the data frames themselves
  expect_false(any(c("icd9", "icd9cm", "icd9who") %in% class(vermont_dx)))
  expect_false(
    any(c("icd10", "icd10cm", "icd10who") %in% class(uranium_pathology))
  )
})

test_that("uranium data looks okay", {
  # generating uranium data depends on RODBC and an access database source file,
  # so don't do this.
  expect_equal(dim(uranium_pathology), c(2376, 2))
  expect_true(icd::is.decimal_diag(uranium_pathology$icd10))
  expect_true(icd::is.icd10(uranium_pathology$icd10))
})

test_that("vermont data looks okay", {
  expect_true(icd::is.icd_wide_data(vermont_dx))
  expect_equal(ncol(vermont_dx), 25)
  expect_gte(nrow(vermont_dx), 1000)
  expect_true(
    all(
      icd::is_valid(
        icd::wide_to_long(vermont_dx)$icd_code
      )
    )
  )
  expect_true(
    all(
      icd::wide_to_long(vermont_dx)$icd_code %in% icd9cm_hierarchy$code
    )
  )
})
