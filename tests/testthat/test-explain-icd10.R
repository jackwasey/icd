context("explain ICD-10-CM codes")
# see also test-explain-who
test_that("basic explain ICD-10 codes", {
  i10 <- list("A00" = "Cholera",
              "Z998" = "Dependence on other enabling machines and devices")
  for (n in names(i10)) {
    expect_identical(x <- explain_code(n), i10[[n]])
    expect_identical(explain_code(as.icd10(n)), x)
    expect_identical(explain_code(as.icd10cm(n)), x)
  }
})
