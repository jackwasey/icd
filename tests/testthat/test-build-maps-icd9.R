context("build icd9 maps")

skip_slow("Skipping re-building of ICD-9 comorbidity maps")

test_that("AHRQ children same as saved", {
  for (i in icd9_map_ahrq)
    expect_equal(
      children.icd9(i, defined = FALSE, short_code = TRUE),
      sort_icd.icd9(i))
})

test_that("Elixhauser children same as saved", {
  for (i in icd9_map_quan_elix)
    expect_equal(
      children.icd9(i, defined = FALSE, short_code = TRUE),
      sort_icd.icd9(i))
})

test_that("Quan Charlson children same as saved", {
  for (i in icd9_map_quan_deyo)
    expect_equal_no_class_order(
      children.icd9(i, defined = FALSE, short_code = TRUE), sort_icd.icd9(i))
})

test_that("Quan Elixhauser children same as saved", {
  for (i in icd9_map_quan_elix)
    expect_equal_no_class_order(
      children.icd9(i, defined = FALSE, short_code = TRUE), sort_icd.icd9(i))
})


test_that("ahrq icd9 map recreated", {
  # skip this test if the file is not already in data-raw
  if (is.null(icd9_fetch_ahrq_sas(offline = TRUE)))
    skip("comformat2012-2013.txt must be downloaded with icd9_fetch_ahrq_sas")
  # same but from source data. Should be absolutely identical.
  expect_identical(
    result <- icd9_parse_ahrq_sas(save_data = FALSE), icd9_map_ahrq)
  expect_is(result, "list")
  expect_equal(length(result), 30)
  expect_equivalent(get_invalid.comorbidity_map(icd9_map_ahrq), list())
})

test_that("Quan Charlson icd9 map generated = saved", {
  if (is.null(icd9_fetch_quan_deyo_sas(offline = TRUE)))
    skip("ICD9_E_Charlson.sas must be downloaded with icd9_fetch_quan_deyo_sas")
  expect_equivalent(
    icd9_map_quan_deyo, icd9_parse_quan_deyo_sas(save_data = FALSE))
  expect_equivalent(
    get_invalid.comorbidity_map(icd9_map_quan_deyo, short_code = TRUE),
    list())
})

test_that("Quan Elix icd9 map generated = saved", {
  expect_equivalent(icd9_map_quan_elix,
                    icd9_generate_map_quan_elix(save_data = FALSE))
  expect_equivalent(
    get_invalid.comorbidity_map(icd9_map_quan_elix, short_code = TRUE),
    list())
})

test_that("Elixhauser icd9 map generated = saved", {
  expect_equivalent(
    icd9_map_elix,
    icd9_generate_map_elix(save_data = FALSE))
  expect_equivalent(
    get_invalid.comorbidity_map(icd9_map_elix, short_code = TRUE),
    list())
})

test_that("Elixhauser icd10 map generated = saved", {
  expect_equivalent(
    icd10_map_elix, icd10_generate_map_elix(save_data = FALSE))
  expect_equivalent(
    get_invalid.comorbidity_map(icd10_map_elix, short_code = TRUE), list())
})
