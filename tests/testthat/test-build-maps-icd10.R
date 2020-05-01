context("build icd10 maps")

test_that("the icd-10 quan elix comorbidity map is reproduced", {
  skip_multi()
  expect_equivalent(
    icd10_map_quan_elix,
    icd10_generate_map_quan_elix(save_pkg_data = FALSE)
  )
})

test_that("the icd-10 quan deyo comorbidity map is reproduced", {
  skip_multi()
  expect_equivalent(
    icd10_map_quan_deyo,
    icd10_generate_map_quan_deyo(save_pkg_data = FALSE)
  )
})

test_that("the icd-10 elix comorbidity map is reproduced", {
  skip_multi()
  expect_equivalent(
    icd10_map_elix,
    icd10_generate_map_elix(save_pkg_data = FALSE)
  )
  expect_equivalent(
    get_invalid.comorbidity_map(icd10_map_elix, short_code = TRUE), list()
  )
})

test_that("icd-10 ahrq map is reproduced", {
  skip_if_offline()
  skip_on_appveyor()
  skip_on_travis()
  skip_on_cran()
  if (is.null(icd10_fetch_ahrq_sas())) {
    skip("AHRQ ICD-10 SAS must be downloaded with icd10_fetch_ahrq_sas")
  }
  expect_identical(
    icd10_map_ahrq,
    icd10_parse_ahrq_sas(save_pkg_data = FALSE,version = "2018")
  )

  expect_false(identical(
    icd10_map_ahrq,
    icd10_parse_ahrq_sas(save_pkg_data = FALSE,version = "2016")
  ))

})
