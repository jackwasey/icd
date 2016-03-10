context("generate defined child codes for ICD-10-CM")

expect_icd10cm_child_is_self <- function(x) {
  for (i in x)
  eval(bquote(expect_identical(icd_children(icd10cm(.(i))), icd10cm(.(i)))))
}

test_that("children of a leaf node returns itself", {

  expect_icd10cm_child_is_self(c("O9A119", "O9A53", "S00.00XA", "T3299", "P150", "P159", "Z9989", "Z950", "C7A098", "C7A8"))

  rand_icd10cm <- generate_random_icd10cm_billable(50)
  expect_icd10cm_child_is_self(rand_icd10cm)

})

