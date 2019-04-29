context("PCCC")
# pccc_col_names <- c(
#   "neuromusc", "cvd", "respiratory", "renal", "gi",
#   "hemato_immu", "metabolic", "congeni_genetic",
#   "malignancy", "neonatal", "tech_dep", "transplant"
# )

test_that("procedure codes work", {
  res9 <- comorbid_pccc_pcs(pccc_pts,
    icd_name = "icd9_pcs",
    return_binary = FALSE
  )
  expect_true(res9[1, "Neuromusc"])
  expect_true(res9[3, "CVD"])
  expect_true(res9["11", "Respiratory"])
  expect_true(res9[3, "TechDep"])
  expect_equal(sum(res9), 4)
  res0 <- comorbid_pccc_pcs(pccc_pts,
    icd_name = "icd10_pcs",
    return_binary = FALSE
  )
  expect_true(res0["11", "CVD"])
  expect_true(res0["10", "Respiratory"])
  expect_true(res0[3, 11])
  expect_true(res0[3, 4])
  expect_true(res0[2, "Transplant"])
  expect_equal(sum(res0), 5)

  # All ICD-9 procedure codes are numeric, some ICD-10 procedure codes
  # are numeric, so best to call functions directly:
  pts <- data.frame(encounters = c(100), icd10_pcs = c("0016070"))
  icd10_comorbid_pccc_pcs(pts, icd_name = "icd10_pcs")
})

test_that("PCCC dx works", {
  res <- icd9_comorbid_pccc_dx(vermont_dx, return_binary = TRUE)
  expect_equivalent(
    colSums(res[1:1000, ]),
    c(82, 270, 50, 119, 55, 39, 313, 30, 128, 7, 129, 21)
  )
  expect_equal(colnames(res), unlist(unname(names_pccc_abbrev)))
})

test_that("colnames same for both dx and procedure codes", {
  expect_warning(
    res9d <- comorbid_pccc_dx(pccc_pts, icd_name = "icd9_dx"),
    regexp = NA
  )

  expect_warning(
    res0d <- comorbid_pccc_dx(pccc_pts, icd_name = "icd10_dx"),
    regexp = NA
  )
  expect_warning(
    res9p <- comorbid_pccc_pcs(pccc_pts, icd_name = "icd9_pcs"),
    regexp = NA
  )
  expect_warning(
    res0p <- comorbid_pccc_pcs(pccc_pts, icd_name = "icd10_pcs"),
    regexp = NA
  )
  expect_identical(names(res9d), names(res9p))
  expect_identical(names(res9d), names(res0d))
  expect_identical(names(res9d), names(res0p))

  expect_warning(
    res9dd <- icd9_comorbid_pccc_dx(pccc_pts, icd_name = "icd9_dx"),
    regexp = NA
  )
  expect_warning(
    res0dd <- icd10_comorbid_pccc_dx(pccc_pts, icd_name = "icd10_dx"),
    regexp = NA
  )
  expect_warning(
    res9pp <- icd9_comorbid_pccc_pcs(pccc_pts, icd_name = "icd9_pcs"),
    regexp = NA
  )
  expect_warning(
    res0pp <- icd10_comorbid_pccc_pcs(pccc_pts, icd_name = "icd10_pcs"),
    regexp = NA
  )
  expect_identical(names(res9d), names(res9dd))
  expect_identical(names(res9d), names(res9pp))
  expect_identical(names(res9d), names(res0dd))
  expect_identical(names(res9d), names(res0pp))
})

test_that("binary vs logical and df output for PCCC", {
  res_bin <- comorbid_pccc_dx(random_test_patients,
    return_binary = TRUE, return_df = TRUE
  )
  res_log <- comorbid_pccc_dx(random_test_patients,
    return_binary = FALSE, return_df = TRUE
  )
  expect_true(all(vapply(res_bin[-1], is.integer, logical(1))))
  expect_true(all(vapply(res_log[-1], is.logical, logical(1))))
  expect_identical(res_bin, logical_to_binary(res_log))
  expect_identical(res_log, binary_to_logical(res_bin))
})
