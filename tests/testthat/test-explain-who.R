context("explain WHO English codes")

test_that("some codes not in ICD-10-CM", {
  # For testing when icd.data may be wrong version:
  skip_missing_icd10who()
  for (hiv in c(
    "B20", "B21", "B22", "B23", "B24",
    "B211",
    "B21.1",
    "B219",
    "B21.9",
    "B22.7", "B238", "Z21"
  )) {
    info <- paste("HIV code: ", hiv)
    expect_error(
      regexp = NA,
      x <- explain_code(as.icd10who(hiv), lang = "en"),
      info = info
    )
    expect_true(length(x) == 1, info = info)
    # workaround https://github.com/r-lib/testthat/issues/867
    expect_true(
      all(
        grepl("HIV", x)
      ),
      info = info
    )
  }
})

test_that("hand-picked WHO-only codes okay", {
  skip_missing_icd10who()
  expect_identical(
    explain_code(as.icd10who("U842")),
    "Resistance to antiviral drug(s)"
  )
})

context("explain WHO French codes")

test_that("some WHO codes are not in ICD-10-CM", {
  # For testing when icd.data may be wrong version:
  skip_missing_icd10who()
  # https://icd.who.int/browse10/2008/fr#/Z21
  # https://icd.who.int/browse10/2008/fr#/B21.9 (.0 .1 .2 .3 .7 .8 .9)
  for (hiv in c(
    "B20", "B21", "B22", "B23", "B24",
    "B21.9", "B22.7", "B238", "Z21"
  )) {
    expect_error(
      regexp = NA,
      x <- explain_code(as.icd10who(hiv), lang = "fr"),
      info = paste("VIH (HIV) code: ", hiv)
    )
    # workaround https://github.com/r-lib/testthat/issues/867
    expect_true(length(x) && all(grepl("VIH", x)),
      info = paste("VIH (HIV) code: ", hiv)
    )
  }
})

test_that("hand-picked WHO-only codes okay", {
  skip_missing_icd10who(ver = "2008", lang = "fr")
  expect_identical(
    explain_code(as.icd10who("F21"), lang = "fr"),
    "Trouble schizotypique"
  )
})
