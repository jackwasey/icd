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

  n <- names(icd9ChaptersMajor)
  # pick out the troublemakers found in testing, and some edge cases.
  expect_equal(icd9ChaptersMajor[["059"]], "Other poxvirus infections")
  expect_equal(icd9ChaptersMajor[["537"]], "Other disorders of stomach and duodenum")
  expect_equal(icd9ChaptersMajor[["538"]], "Gastrointestinal mucositis (ulcerative)")
  expect_equal(icd9ChaptersMajor[["777"]], "Perinatal disorders of digestive system")
  # some have no subchapter, just major:
  expect_false("280" %in% names(icd9ChaptersSub))
  expect_false("289" %in% names(icd9ChaptersSub))
  expect_false("740" %in% names(icd9ChaptersSub))
  expect_false("759" %in% names(icd9ChaptersSub))
  expect_equal(icd9ChaptersMajor[["280"]], "Iron deficiency anemias")
  expect_equal(icd9ChaptersMajor[["289"]], "Other diseases of blood and blood-forming organs")
  expect_equal(icd9ChaptersMajor[["740"]], "Anencephalus and similar anomalies")
  expect_equal(icd9ChaptersMajor[["759"]], "Other and unspecified congenital anomalies")

  #expect_equal(icd9ChaptersMajor[[""]], "")

  # all subs and chapters should be ranges:
  expect_match(names(icd9Chapters), "^[VvEe[:digit:]]*-[VvEe[:digit:]]*$")
  expect_match(names(icd9ChaptersSub), "^[VvEe[:digit:]]*-[VvEe[:digit:]]*$")
  expect_match(names(icd9Chapters), "[VvEe[:digit:]]*")

  expect_true(all(icd9ValidMajor(names(icd9ChaptersMajor))))
})

test_that("icd9ChaptersMajor - positive values", {
  n <- names(icd9ChaptersMajor)
  expect_true("001" %in% n)
  expect_true("V01" %in% n)
  expect_true("V91" %in% n) # only up to V89 in 2011.
  expect_true("E000" %in% n)
  expect_true("E001" %in% n)
  expect_true("E009" %in% n)
  expect_true("E013" %in% n)
  expect_true("E016" %in% n)
  expect_true("E849" %in% n)
  expect_true("E917" %in% n)
})

test_that("icd9ChaptersMajor - negative values", {
  n <- names(icd9ChaptersMajor)
  # there are some gaps: just make sure we don't have any spurious codes:
  expect_false("888" %in% n)
  expect_false("889" %in% n)
  expect_false("V00" %in% n)
  expect_false("V38" %in% n)
  expect_false("E777" %in% n)
})

for (i in list("icd9Chapters", "icd9ChaptersSub", "icd9ChaptersMajor")) {
  il <- get(i)
  test_that(paste("icd9Chapters... duplication, blanks: ", i), {
    expect_equal(length(il), length(unique(names(il)))) # should be no duplicate names
    expect_equal(length(il), length(unique(il))) # or duplicate codes
    expect_true(all(nchar(il > 0)))
    expect_true(all(nchar(names(il) > 0)))
    expect_false(any(is.na(il)))
    expect_false(any(is.na(names(il))))
  })
}

test_that("parse icd9ChaptersMajor vs those listed in the other CDC source of the leaf definitions.", {
  library(magrittr, quietly = T, warn.conflicts = F)
  # get all the majors from the other list, to compare
  compareMajors <- icd9CmDesc$icd9 %>% icd9ShortToMajor %>% unique
  expect_true(all(compareMajors %in% names(icd9ChaptersMajor)))
  expect_true(all(names(icd9ChaptersMajor) %in% compareMajors))
})

# this is hand written: use to verify top level of the web site scrape: TODO
testChapters <- list(
  "Infectious And Parasitic Diseases" = c(start = "001", end = "139"),
  "Neoplasms" = c(start = "140", end = "239"),
  "Endocrine, Nutritional And Metabolic Diseases, And Immunity Disorders" = c(start = "240", end = "279"),
  "Diseases Of The Blood And Blood-Forming Organs" = c(start = "280", end = "289"),
  "Mental Disorders" = c(start = "290", end = "319"),
  "Diseases Of The Nervous System And Sense Organs" = c(start = "320", end = "389"),
  "Diseases Of The Circulatory System" = c(start = "390", end = "459"),
  "Diseases Of The Respiratory System" = c(start = "460", end = "519"),
  "Diseases Of The Digestive System" = c(start = "520", end = "579"),
  "Diseases Of The Genitourinary System" = c(start = "580", end = "629"),
  "Complications Of Pregnancy, Childbirth, And The Puerperium" = c(start = "630", end = "679"),
  "Diseases Of The Skin And Subcutaneous Tissue" = c(start = "680", end = "709"),
  "Diseases Of The Musculoskeletal System And Connective Tissue" = c(start = "710", end = "739"),
  "Congenital Anomalies" = c(start = "740", end = "759"),
  "Certain Conditions Originating In The Perinatal Period" = c(start = "760", end = "779"),
  "Symptoms, Signs, And Ill-Defined Conditions" = c(start = "780", end = "799"),
  "Injury And Poisoning" = c(start = "800", end = "999"),
  "Supplementary Classification Of Factors Influencing Health Status And Contact With Health Services" = c(start = "V01", end = "V99"),
  "Supplementary Classification Of External Causes Of Injury And Poisoning  " = c(start = "E000", end = "E999")
)
