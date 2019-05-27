context("ICD-9 map children all present")

skip_slow("Skipping slow checking of ICD-9 comorbidity map children (elix)")

test_that("AHRQ children same as saved", {
  for (i in icd9_map_ahrq) {
    expect_equal(
      children.icd9(i, defined = FALSE, short_code = TRUE),
      sort.icd9(i)
    )
  }
})

test_that("Elixhauser children same as saved", {
  for (i in icd9_map_quan_elix) {
    expect_equal(
      children.icd9(i, defined = FALSE, short_code = TRUE),
      sort.icd9(i)
    )
  }
})
