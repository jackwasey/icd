context("chapter and sub-chapters")

test_that("sub-chapters account for ICD-10-CM ordering quirks", {
  # C7A Malignant Neuroendocrine Tumors
  # C7B Secondary Neuroendocrine Tumors
  # D3A Benign Neuroendocrine Tumors
  expect_lt(
    which(names(icd10_sub_chapters) == "Malignant Neoplasms Of Ill-Defined, Other Secondary And Unspecified Sites"), # nolint
    which(names(icd10_sub_chapters) == "Malignant Neuroendocrine Tumors")
  )
  expect_lt(
    which(names(icd10_sub_chapters) == "Malignant Neoplasms Of Ill-Defined, Other Secondary And Unspecified Sites"), # nolint
    which(names(icd10_sub_chapters) == "Secondary Neuroendocrine Tumors")
  )
  expect_lt(
    which(names(icd10_sub_chapters) == "Neoplasms Of Uncertain Behavior, Polycythemia Vera And Myelodysplastic Syndromes"), # nolint
    which(names(icd10_sub_chapters) == "Benign Neuroendocrine Tumors")
  )
  expect_gt(
    which(names(icd10_sub_chapters) == "Malignant Neoplasms Of Lymphoid, Hematopoietic And Related Tissue"), # nolint
    which(names(icd10_sub_chapters) == "Malignant Neuroendocrine Tumors")
  )
  expect_gt(
    which(names(icd10_sub_chapters) == "Malignant Neoplasms Of Lymphoid, Hematopoietic And Related Tissue"), # nolint
    which(names(icd10_sub_chapters) == "Secondary Neuroendocrine Tumors")
  )
  expect_gt(
    which(names(icd10_sub_chapters) == "Neoplasms Of Unspecified Behavior"), # nolint
    which(names(icd10_sub_chapters) == "Benign Neuroendocrine Tumors")
  )
})
