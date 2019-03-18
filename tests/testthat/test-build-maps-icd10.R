context("build icd10 maps")

test_that("the icd-10 quan elix comorbidity map is reproduced", {
  expect_equivalent(icd10_map_quan_elix, icd10_generate_map_quan_elix(save_data = FALSE))
})

test_that("the icd-10 quan deyo comorbidity map is reproduced", {
  expect_equivalent(icd10_map_quan_deyo, icd10_generate_map_quan_deyo(save_data = FALSE))
})

test_that("the icd-10 elix comorbidity map is reproduced", {
  expect_equivalent(icd10_map_elix, icd10_generate_map_elix(save_data = FALSE))
})

test_that("icd-10 ahrq map is reproduced", {
  skip_if_not_installed("icd.data", "1.1.2")
  if (is.null(icd10_fetch_ahrq_sas(offline = TRUE))) {
    skip("AHRQ ICD-10 SAS must be downloaded with icd10_fetch_ahrq_sas")
  }
  expect_equivalent(icd10_map_ahrq, icd10_parse_ahrq_sas(save_data = FALSE))
})
