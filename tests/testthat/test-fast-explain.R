context("explain ICD-9: code to human-readable")

test_that("explain a large set of ICD-9 codes succinctly", {
  expect_identical(
    icd9ExplainShort(icd9ChildrenShort("391", onlyReal = FALSE),
                     condense = FALSE),
    c("Rheumatic fever with heart involvement", "Acute rheumatic pericarditis",
      "Acute rheumatic endocarditis", "Acute rheumatic myocarditis",
      "Other acute rheumatic heart disease",
      "Acute rheumatic heart disease, unspecified")
  )

  #TODO same but condensing
  expect_identical(
    icd9ExplainShort(icd9ChildrenShort("391"), condense = TRUE),
    "Rheumatic fever with heart involvement"
  )

})

test_that("explain single top level code which is billable, has no children", {
  # the code "390" is a billable major: good test case.
  expect_identical(icd9ExplainShort("390"),
                   "Rheumatic fever without mention of heart involvement")
})

test_that("expalin a single top level code without a top level explanation", {
  expect_identical(icd9ExplainShort("391"),
                   "Rheumatic fever with heart involvement")
})


test_that("explain a single leaf node" , {
  expect_equal(icd9ExplainShort("27800", condense = FALSE),
               "Obesity, unspecified")
  expect_equal(icd9ExplainShort("27800", condense = TRUE),
               "Obesity, unspecified")
})

# TODO:
# use cases for explaining ICD-9 codes:
test_that("given ICD-9 codes all have human-readable explanations", {
})
test_that("some valid ICD-9 codes are human-readable, some aren't", {
})
test_that("some valid ICD-9 codes are human-readable,
          but some aren't, and some are invalid", {
          })
test_that("none ICD-9 codes have human-readable explanations,
          but are all valid", {
          })
test_that("none ICD-9 codes are even valid", {
})


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
})

# TODO, set up long chain of multiple conversions as kind of integration test,
# and to flush out errors.

test_that("extract top-level codes from the RTF gives the complete list", {
  # total number of codes
  # pick out a few at random
  # all valid major form.

  # pick out the troublemakers found in testing, and some edge cases.
  expect_equal(icd9ChaptersMajor[["Other poxvirus infections"]],
               structure("059", .Names = "major"))
  expect_equal(icd9ChaptersMajor[["Other disorders of stomach and duodenum"]],
               structure("537", .Names = "major"))
  expect_equal(icd9ChaptersMajor[["Gastrointestinal mucositis (ulcerative)"]],
               structure("538", .Names = "major"))
  expect_equal(icd9ChaptersMajor[["Perinatal disorders of digestive system"]],
               structure("777", .Names = "major"))
  # some have no subchapter, just major:
  expect_false("280" %in% icd9ChaptersSub)
  expect_false("289" %in% icd9ChaptersSub)
  expect_false("740" %in% icd9ChaptersSub)
  expect_false("759" %in% icd9ChaptersSub)
  expect_equal(
    icd9ChaptersMajor[["Iron deficiency anemias"]],
    structure("280", .Names = "major"))
  expect_equal(
    icd9ChaptersMajor[["Other diseases of blood and blood-forming organs"]],
    structure("289", .Names = "major"))
  expect_equal(
    icd9ChaptersMajor[["Anencephalus and similar anomalies"]],
    structure("740", .Names = "major"))
  expect_equal(
    icd9ChaptersMajor[["Other and unspecified congenital anomalies"]],
    structure("759", .Names = "major"))

  #expect_equal(icd9ChaptersMajor[[""]], "")

  # format of each hierarchy level:
  expect_equal(names(icd9Chapters[[1]]), c("start", "end"))
  expect_equal(names(icd9ChaptersSub[[1]]), c("start", "end"))
  expect_equal(names(icd9ChaptersMajor[[1]]), c("major"))
  # should be no NA values
  expect_true(all(!is.na(vapply(icd9Chapters, "[[", FUN.VALUE="", 1))))
  expect_true(all(!is.na(vapply(icd9Chapters, "[[", FUN.VALUE="", 2))))
  expect_true(all(!is.na(vapply(icd9ChaptersSub, "[[", FUN.VALUE="", 1))))
  expect_true(all(!is.na(vapply(icd9ChaptersSub, "[[", FUN.VALUE="", 2))))
  expect_true(all(!is.na(vapply(icd9ChaptersMajor, "[[", FUN.VALUE="", 1))))

  # all the range limits and majors should be valid majors
  expect_true(
    all(icd9ValidMajor(vapply(icd9Chapters, "[[", FUN.VALUE="", 1))))
  expect_true(
    all(icd9ValidMajor(vapply(icd9Chapters, "[[", FUN.VALUE="", 2))))
  expect_true(
    all(icd9ValidMajor(vapply(icd9ChaptersSub, "[[", FUN.VALUE="", 1))))
  expect_true(
    all(icd9ValidMajor(vapply(icd9ChaptersSub, "[[", FUN.VALUE="", 2))))
  expect_true(
    all(icd9ValidMajor(vapply(icd9ChaptersMajor, "[[", FUN.VALUE="", 1))))
})

test_that("icd9ChaptersMajor - positive values", {
  expect_true("001" %in% icd9ChaptersMajor)
  expect_true("V01" %in% icd9ChaptersMajor)
  expect_true("V91" %in% icd9ChaptersMajor) # only up to V89 in 2011.
  expect_true("E000" %in% icd9ChaptersMajor)
  expect_true("E001" %in% icd9ChaptersMajor)
  expect_true("E009" %in% icd9ChaptersMajor)
  expect_true("E013" %in% icd9ChaptersMajor)
  expect_true("E016" %in% icd9ChaptersMajor)
  expect_true("E849" %in% icd9ChaptersMajor)
  expect_true("E917" %in% icd9ChaptersMajor)
})

test_that("icd9ChaptersMajor - negative values", {
  # there are some gaps: just make sure we don't have any spurious codes:
  expect_false("888" %in% icd9ChaptersMajor)
  expect_false("889" %in% icd9ChaptersMajor)
  expect_false("V00" %in% icd9ChaptersMajor)
  expect_false("V38" %in% icd9ChaptersMajor)
  expect_false("E777" %in% icd9ChaptersMajor)
})

for (i in list("icd9Chapters", "icd9ChaptersSub", "icd9ChaptersMajor")) {
  il <- get(i)
  test_that(paste("icd9Chapters... duplication, blanks: ", i), {
    # should be no duplicate names
    expect_equal(length(il), length(unique(names(il))))
    # or duplicate codes
    expect_equal(length(il), length(unique(il)))
    expect_true(all(nchar(names(il) > 0)))
    expect_false(any(is.na(il)))
    expect_false(any(is.na(names(il))))
  })
}

test_that("parse icd9ChaptersMajor vs those listed
          in the other CDC source of the leaf definitions.", {
            # get all the majors from the other list, to compare

            compareMajors <- unique(icd9GetMajor(icd9::icd9Hierarchy$icd9, isShort = TRUE))
            expect_true(all(compareMajors %in% icd9ChaptersMajor))
            expect_true(all(icd9ChaptersMajor %in% compareMajors))
          })

test_that("unsorted hierarchy tests", {
  expect_equal(
    tolower(icd9::icd9Hierarchy[icd9Hierarchy[["icd9"]] == "00321",
                                "descLong"]),
    tolower("Salmonella Meningitis"))
})

# this is hand written: use to verify top level of the web site scrape: TODO
testChapters <- list(
  "Infectious And Parasitic Diseases" = c(start = "001", end = "139"),
  "Neoplasms" = c(start = "140", end = "239"),
  "Endocrine, Nutritional And Metabolic Diseases, And Immunity Disorders" =
    c(start = "240", end = "279"),
  "Diseases Of The Blood And Blood-Forming Organs" =
    c(start = "280", end = "289"),
  "Mental Disorders" = c(start = "290", end = "319"),
  "Diseases Of The Nervous System And Sense Organs" =
    c(start = "320", end = "389"),
  "Diseases Of The Circulatory System" = c(start = "390", end = "459"),
  "Diseases Of The Respiratory System" = c(start = "460", end = "519"),
  "Diseases Of The Digestive System" = c(start = "520", end = "579"),
  "Diseases Of The Genitourinary System" = c(start = "580", end = "629"),
  "Complications Of Pregnancy, Childbirth, And The Puerperium" =
    c(start = "630", end = "679"),
  "Diseases Of The Skin And Subcutaneous Tissue" =
    c(start = "680", end = "709"),
  "Diseases Of The Musculoskeletal System And Connective Tissue" =
    c(start = "710", end = "739"),
  "Congenital Anomalies" = c(start = "740", end = "759"),
  "Certain Conditions Originating In The Perinatal Period" =
    c(start = "760", end = "779"),
  "Symptoms, Signs, And Ill-Defined Conditions" = c(start = "780", end = "799"),
  "Injury And Poisoning" = c(start = "800", end = "999"),
  "Supplementary Classification Of Factors Influencing \
  Health Status And Contact With Health Services" =
    c(start = "V01", end = "V99"),
  "Supplementary Classification Of External Causes Of Injury And Poisoning  " =
    c(start = "E000", end = "E999")
)

test_that("condense single major and its children", {
  expect_equal(icd9CondenseShort("391"),
               "Rheumatic fever with heart involvement")
  expect_equal(icd9CondenseShort(icd9ChildrenShort("391")),
               "Rheumatic fever with heart involvement")
  expect_equal(icd9CondenseShort(icd9ChildrenShort("391", onlyReal = TRUE)),
               "Rheumatic fever with heart involvement")
})

test_that("condense short range", {

  othersalmonella <- c("0030", "0031", "00320", "00321", "00322",
                       "00323", "00324", "00329", "0038", "0039")

  expect_equal(icd9CondenseShort(icd9Short = othersalmonella),
               "Other salmonella infections")
  expect_equal(icd9CondenseShort(icd9Short = othersalmonella[-3]),
               icd9Hierarchy[c(9, 10, 12:18), "descLong"])

  expect_equal(icd9CondenseToMajor(othersalmonella, onlyReal = TRUE), "003")
  expect_equal(icd9CondenseToMajor(othersalmonella, onlyReal = FALSE),
               othersalmonella)
  expect_equal(icd9CondenseToMajor(othersalmonella[-3], onlyReal = TRUE),
               othersalmonella[-3])
  expect_equal(icd9CondenseToMajor(othersalmonella[-3], onlyReal = FALSE),
               othersalmonella[-3])

  expect_equal(sort(icd9ChildrenShort(icd9Short = "001", onlyReal = TRUE)),
               c("0010", "0011", "0019"))



})

test_that("explain icd9GetChapters bad input", {})

test_that("explain icd9GetChapters simple input", {
  chaps1 <- icd9GetChapters(c("410", "411", "412"), isShort = TRUE)
  expect_equal(nrow(chaps1), 3)

  chaps2 <- icd9GetChapters("418", isShort = TRUE)
  expect_is(chaps2, "data.frame")
  expect_is(chaps2$threedigit, "factor")
  expect_is(chaps2$major, "factor")
  expect_is(chaps2$subchapter, "factor")
  expect_is(chaps2$chapter, "factor")
  expect_equal(asCharacterNoWarn(chaps2$threedigit), NA_character_)
  expect_equal(asCharacterNoWarn(chaps2$major), NA_character_)
  expect_equal(asCharacterNoWarn(chaps2$subchapter), NA_character_)
  expect_equal(asCharacterNoWarn(chaps2$chapter), NA_character_)

  chaps3 <- icd9GetChapters("417", isShort = FALSE)
  expect_equal(asCharacterNoWarn(chaps3$threedigit), "417")
  expect_equal(asCharacterNoWarn(chaps3$major),
               "Other diseases of pulmonary circulation")
  expect_equal(asCharacterNoWarn(chaps3$subchapter),
               "Diseases Of Pulmonary Circulation")
  expect_equal(asCharacterNoWarn(chaps3$chapter),
               "Diseases Of The Circulatory System")

  chaps4 <- icd9GetChapters("417", isShort = TRUE)
  chaps5 <- icd9GetChapters("417.1", isShort = FALSE)
  chaps6 <- icd9GetChapters("4171", isShort = TRUE)
  chaps7 <- icd9GetChapters("417.1", isShort = FALSE)
  chaps8 <- icd9GetChapters("4171", isShort = TRUE)
  expect_equal(chaps3, chaps4)
  expect_equal(chaps3, chaps5)
  expect_equal(chaps3, chaps6)
  expect_equal(chaps3, chaps7)
  expect_equal(chaps3, chaps8)

  #TODO: decide whether to give NA rows if the major group doesn't exist, or
  #whether also to do this for non-existent minor parts. E.g. 41710 doesn't
  #exist, but 4171 does. Should we give a NA row back or not?
})

