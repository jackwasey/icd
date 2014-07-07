context("explain ICD-9: code to human-readable")

test_that("explain a large set of ICD-9 codes succinctly", {
  expect_identical(
    icd9ExplainShort(icd9ChildrenShort("391")),
    structure(
      list(
        ICD9 = c("3910", "3911", "3912", "3918", "3919"),
        Diagnosis = c("Acute rheumatic pericarditis", "Acute rheumatic endocarditis",
                      "Acute rheumatic myocarditis", "Other acute rheumatic heart disease",
                      "Acute rheumatic heart disease, unspecified"),
        Description = c("Acute rheumatic pericard",
                        "Acute rheumatic endocard", "Ac rheumatic myocarditis", "Ac rheumat hrt dis NEC",
                        "Ac rheumat hrt dis NOS")),
      .Names = c("ICD-9", "Diagnosis", "Description"),
      row.names = c(NA, -5L),
      class = "data.frame"
    )
  )
})

test_that("explain a single top level ICD-9 code which does have an explanation", {
  expect_identical(
    icd9ExplainShort("390"),
    structure(
      list(
        ICD9 = "390",
        Diagnosis = " Rheumatic fever without mention of heart involvement",
        Description = " Rheum fev w/o hrt involv"
      ),
      .Names = c("ICD-9", "Diagnosis", "Description"),
      row.names = c(NA, -1L),
      class = "data.frame"
    )
  )
})

test_that("expalin a single top level ICD-9 code without a top level explanation", {
  expect_identical(
    icd9ExplainShort("391"),
    structure(
      list(
        'ICD-9' = c("3910", "3911", "3912", "3918", "3919"),
        Diagnosis = c("Acute rheumatic pericarditis", "Acute rheumatic endocarditis",
                      "Acute rheumatic myocarditis", "Other acute rheumatic heart disease",
                      "Acute rheumatic heart disease, unspecified"),
        Description = c("Acute rheumatic pericard", "Acute rheumatic endocard",
                        "Ac rheumatic myocarditis", "Ac rheumat hrt dis NEC",
                        "Ac rheumat hrt dis NOS")
      ),
      .Names = c("ICD-9", "Diagnosis", "Description"),
      row.names = c(NA, -5L),
      class = "data.frame"
    )
  )
})

#TODO, set up long chain of multiple conversions as kind of integration test, and to flush out errors.
