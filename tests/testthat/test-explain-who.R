context("explain WHO codes")

test_that("some codes not in ICD-10-CM", {
  # For testing when icd.data may be wrong version:
  skip_if_not_installed("icd.data", 1.1)
  icd.data::skip_missing_icd10who2016()
  for (hiv in c("B20", "B21", "B22", "B23", "B24",
                "B21.9", "B22.7", "B238", "Z21")) {
    expect_match(x <- explain_code(as.icd10who(hiv)), "HIV")
    expect_length(x, 1)
  }
})
