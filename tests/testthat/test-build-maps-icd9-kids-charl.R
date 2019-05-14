context("ICD-9 map children all present")

skip_slow("Skipping slow checking of ICD-9 comorbidity map children (charl)")

test_that("Quan Charlson children same as saved", {
  for (i in icd9_map_quan_deyo) {
    expect_equal_no_class_order(
      children.icd9(i, defined = FALSE, short_code = TRUE),
      sort.icd9(i)
    )
  }
})
