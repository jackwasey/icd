context("explain WHO English codes")

test_that("some codes not in ICD-10-CM", {
  # For testing when icd.data may be wrong version:
  skip_if_not_installed("icd.data", 1.1)
  skip_missing_icd10who()
  for (hiv in c(
    "B20", "B21", "B22", "B23", "B24",
    "B21.9", "B22.7", "B238", "Z21"
  )) {
    expect_match(x <- explain_code(as.icd10who(hiv)),
      "HIV",
      info = paste("HIV code: ", hiv)
    )
    expect_identical(x, explain_code.icd10who(hiv))
    expect_length(x, 1)
  }
})

test_that("hand-picked WHO-only codes okay", {
  skip_if_not_installed("icd.data", 1.1)
  skip_missing_icd10who()
  expect_identical(
    explain_code.icd10who("U842"),
    "Resistance to antiviral drug(s)"
  )
})

context("explain WHO French codes")

test_that("some codes not in ICD-10-CM", {
  # For testing when icd.data may be wrong version:
  skip_if_not_installed("icd.data", 1.1)
  skip_missing_icd10who()
  for (hiv in c(
    "B20", "B21", "B22", "B23", "B24",
    "B21.9", "B22.7", "B238", "Z21"
  )) {
    expect_match(x <- explain_code(as.icd10who(hiv), lang = "fr"),
      "VIH",
      info = paste("HIV code: ", hiv)
    )
    expect_length(x, 1)
  }
})

test_that("hand-picked WHO-only codes okay", {
  skip_if_not_installed("icd.data", 1.1)
  skip_missing_icd10who()
  expect_identical(
    explain_code.icd10who("F21", lang = "fr"),
    "Trouble schizotypique"
  )
})
