context("test hcc")

# Typical use case
# Data as expected. Multiple visits (different patients encounters)
# One of the patients with multiple visit dates, all valid ICDs
# Returns a unique HCC for each visit/date combination
# Only returns matches for valid ICDs in CC crosswalk
test_that("simple hcc 4 patients, different dates, each single HCC assigned", {
            res <- comorbid_hcc(hcc_test_simple9)
            expect_equal(dim(res), c(4, 3))
            expect_true(setequal(c("visit_id", "date", "hcc"), names(res)))
          })

# Data as expected but only a single record
test_that("hcc one patient, single visit should have a single HCC assigned", {
            res <- comorbid_hcc(hcc_test_single)
            expect_equal(dim(res), c(1, 3))
            expect_true(setequal(c("visit", "date", "hcc"), names(res)))
          })

# Mix of valid and invalid ICDs, some patients dont have any valid ICDs
# Only returns matches for valid ICDs in CC crosswalk
# should return 2 rows, 2 different patients
test_that("hcc 2 pt/visit combos each should have a single HCC assigned", {
            res <- comorbid_hcc(hcc_test_invalid)
            expect_equal(dim(res), c(2, 3))
            expect_true(setequal(c("patient", "date", "hcc"), names(res)))
          })

test_that("github 153", {
  hcc_github153_df <- structure(
    list(date = structure(c(16855, 17123, 17008, NA), class = "Date"),
         visit_id = c("1.98433805210381e-310", "6.92092372368813e-311",
                      "7.85012017519427e-311", NA),
         icd_code = c("F1020", "G4733", "M1712", NA)),
    row.names = c("1", "2", "3", "NA"),
    class = c("icd_long_data", "data.frame"))
  expect_error(regexp = NA, icd_comorbid_hcc(hcc_github153_df))
})

test_that("hcc icd10 codes", {
  res <- icd10_comorbid_hcc(hcc_test_simple10)
  expect_true(nrow(res) > 0)
})

test_that("hcc works with non-standard column names", {
  d2 <- hcc_test_simple9
  names(d2) <- c("a", "b", "c")
  expect_equivalent(
    comorbid_hcc(hcc_test_simple9),
    comorbid_hcc(d2, date_name = "c",
                 visit_name = "a", icd_name = "b"))
})
