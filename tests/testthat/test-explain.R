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

# TODO:
# use cases for explaining ICD-9 codes:
test_that("given ICD-9 codes all have human-readable explanations", {})
test_that("some of the given valid ICD-9 codes have human-readable explanations, but some don't", {})
test_that("some of the given valid ICD-9 codes have human-readable explanations, but some don't, and some are invalid", {})
test_that("none of the given ICD-9 codes have human-readable explanations, but are all valid", {})
test_that("none of the given  ICD-9 codes are even valid", {})


test_that("guess icd9 types: short", {
  expect_true(icd9GuessIsShort("12345"))
  expect_true(icd9GuessIsShort("1234"))
})

test_that("guess icd9 types: ambiguous", {
  expect_equal(icd9GuessIsShort("123"), NA)
  expect_equal(icd9GuessIsShort(c("123.4","2345")), NA)
})

test_that("guess icd9 types: decimal", {
  expect_false(icd9GuessIsShort("123.45"))
  expect_false(icd9GuessIsShort("123.4"))
  expect_false(icd9GuessIsShort("123."))
})

test_that("guess icd9 types: invalid", {
  expect_equal(icd9GuessIsShort(NA_character_), NA)
  expect_error(icd9GuessIsShort(NA_character_, invalidAction = "stop"))
  expect_warning(icd9GuessIsShort("mogli", invalidAction = "warn"))
  expect_warning(icd9GuessIsShort(c(100, 200, 300, 400, 500, "mogli"), invalidAction = "warn"))
  expect_warning(icd9GuessIsShort(c(100.1, 200.2, 300.3, 400.4, 500.5, "mogli"), invalidAction = "warn"))
  expect_warning(icd9GuessIsShort(c(1001, 2002, 3003, 4004, 5005, "mogli"), invalidAction = "warn"))
})

#TODO, set up long chain of multiple conversions as kind of integration test, and to flush out errors.

test_that("extraction of top-level ICD-9 codes from the RTF gives the complete list", {
# total number of codes
# pick out a few at random
# all valid major form.

  n <- names(icd9CmMajors)

expect_true("059" %in% n)
expect_equal(unname(icd9CmMajors[n == "059"]), "Other poxvirus infections")
expect_true("538" %in% n)
expect_false("888" %in% n)
expect_false("889" %in% n)
expect_true("001" %in% n)
expect_false("V00" %in% n)
expect_false("V38" %in% n)
expect_true("V01" %in% n)
# V90 and V91 are defined elsewhere, but not in this source RTF. TODO: is there an updated RTF or addendum?
expect_true("V89" %in% n)
expect_true("E000" %in% n)
expect_true("E001" %in% n)
expect_true("E009" %in% n)
expect_true("E013" %in% n)
expect_true("E016" %in% n)
expect_true("E849" %in% n) # this does have special formatting in the rtf doc, so missed by regex
expect_true("E917" %in% n)
expect_false("E777" %in% n)

#TODO: "539"  "E013" "E016" "E849" "E917" "V90"  "V91"
# 010 data goes to V89... I need 2014.
})

test_that("parse icd9CmMajors set tests", {
expect_true(all(icd9ValidMajor(n)))
expect_equal(length(icd9CmMajors), length(unique(n))) # should be no duplicate names
expect_equal(length(icd9CmMajors), length(unique(icd9CmMajors))) # or duplicate codes
# no zero length names:
expect_true(all(nchar(icd9CmMajors > 0)))
expect_true(all(!is.na(icd9CmMajors)))
expect_true(all(!is.na(n)))

 # get all the majors from the other list, to compare
 compareMajors <- icd9CmDesc$icd9 %>% icd9ShortToMajor %>% unique
 expect_that(all(compareMajors %in% n))
 expect_that(all(n %in% compareMajors))

})

