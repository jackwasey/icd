context("build icd9 maps notquan")

skip_slow("Skipping slow re-building of ICD-9 comorbidity maps (not quan)")

test_that("ahrq icd9 map recreated", {
  # skip this test if the file is not already in data-raw
  if (with_offline(
    TRUE,
    with_absent_action(
      "silent",
      is.null(icd9_fetch_ahrq_sas())
    )
  )) {
    skip("comformat2012-2013.txt must be downloaded with icd9_fetch_ahrq_sas()")
  }
  # same but from source data. Should be absolutely identical.
  expect_identical(
    result <- icd9_parse_ahrq_sas(save_pkg_data = FALSE), icd9_map_ahrq
  )
  expect_is(result, "list")
  expect_equal(length(result), 30)
  expect_equivalent(get_invalid.comorbidity_map(icd9_map_ahrq), list())
})

test_that("Elixhauser icd9 map generated = saved", {
  expect_equivalent(
    icd9_map_elix,
    icd9_generate_map_elix(save_pkg_data = FALSE)
  )
  expect_equivalent(
    get_invalid.comorbidity_map(icd9_map_elix, short_code = TRUE),
    list()
  )
})
