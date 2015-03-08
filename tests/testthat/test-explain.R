context("explain ICD-9: code to human-readable")

test_that("explain a large set of ICD-9 codes succinctly", {
  expect_identical(
    icd9ExplainShort(icd9ChildrenShort("391", onlyReal = FALSE),
                     doCondense = FALSE),
    c("Rheumatic fever with heart involvement","Acute rheumatic pericarditis",
      "Acute rheumatic endocarditis", "Acute rheumatic myocarditis",
      "Other acute rheumatic heart disease",
      "Acute rheumatic heart disease, unspecified")
  )
  # TODO: also warn or message if there are codes without explanations

  #TODO same but condensing
  expect_identical(
    icd9ExplainShort(icd9ChildrenShort("391"), doCondense = TRUE),
    "Rheumatic fever with heart involvement"
  )

})

test_that("github issue #41", {
  expect_equal(
    icd9Explain(icd9GetReal(quanDeyoComorbid[["Dementia"]]), doCondense = TRUE),
    icd9Explain(quanDeyoComorbid[["Dementia"]], doCondense = TRUE)
  )
  expect_equal(
    icd9Explain(icd9GetReal(quanDeyoComorbid[["Dementia"]]), doCondense = FALSE),
    icd9Explain(quanDeyoComorbid[["Dementia"]], doCondense = FALSE)
  )
})

test_that("explain S3 dispatch", {
  expect_equal(icd9Explain("003.21", isShort = FALSE),
               "Salmonella meningitis")
  expect_equal(icd9ExplainDecimal("003.21"),
               icd9Explain("003.21", isShort = FALSE))
  expect_equal(icd9Explain.list(list(a = "003.21"), isShort = FALSE),
               list(a=icd9Explain("00321", isShort = TRUE)))
  expect_equal(icd9Explain.list(list(a = "003.21", b= "390"), isShort = FALSE),
               list(a = icd9Explain("00321", isShort = TRUE),
                    b = "Rheumatic fever without mention of heart involvement"))
  expect_warning(res <- icd9Explain(list(a = "not", b = "icd9code"), isShort = TRUE))
  expect_equal(res, list(a = character(0), b = character(0)))
  expect_warning(res <- icd9Explain(list(a = "not", b = "icd9code"), isShort = FALSE))
  expect_equal(res, list(a = character(0), b = character(0)))

  expect_warning(res <- icd9Explain.numeric(3.21, isShort = FALSE))
  expect_equal(res, icd9Explain("00321", isShort = TRUE))

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
  expect_equal(icd9ExplainShort("27800", doCondense = FALSE),
               "Obesity, unspecified")
  expect_equal(icd9ExplainShort("27800", doCondense = TRUE),
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
  expect_true(icd9GuessIsShort(c("12345", "234")))
  # we only look at first one...
  expect_true(icd9GuessIsShort(c("12345", "23.4")))
  expect_true(icd9GuessIsShort("1234"))
})

test_that("guess icd9 types: ambiguous, default to short", {
  expect_equal(icd9GuessIsShort(c("123.4","2345")), TRUE)
  expect_equal(icd9GuessIsShort(c("123.4", NA, "2345")), TRUE)
})

test_that("guess icd9 types: decimal", {
  expect_false(icd9GuessIsShort("123.45"))
  expect_false(icd9GuessIsShort("123.4"))
  expect_false(icd9GuessIsShort("123."))
})

test_that("guess icd9 types: invalid", {
  expect_equal(icd9GuessIsShort(NA_character_), TRUE)
})

test_that("guess with just majors", {
  # it acutally doesn't matter if they are all majors, so we default to 'short'
  # which is usually the most direct route to an answer
  expect_true(icd9GuessIsShort(c("100", "101", "102")))
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
    all(icd9IsValidMajor(vapply(icd9Chapters, "[[", FUN.VALUE="", 1))))
  expect_true(
    all(icd9IsValidMajor(vapply(icd9Chapters, "[[", FUN.VALUE="", 2))))
  expect_true(
    all(icd9IsValidMajor(vapply(icd9ChaptersSub, "[[", FUN.VALUE="", 1))))
  expect_true(
    all(icd9IsValidMajor(vapply(icd9ChaptersSub, "[[", FUN.VALUE="", 2))))
  expect_true(
    all(icd9IsValidMajor(vapply(icd9ChaptersMajor, "[[", FUN.VALUE="", 1))))
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

            compareMajors <- unique(icd9GetMajor(icd9::icd9Hierarchy$icd9,
                                                 isShort = TRUE))
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

test_that("condense full ranges", {
  # condensing to "real" means we don't get a lot of majors, which are often not
  # themselves defined.
  # majors:
  expect_equal(icd9CondenseShort(icd9ChildrenShort("003", onlyReal = FALSE), onlyReal = FALSE), "003")
  expect_equal(icd9CondenseShort(icd9ChildrenShort("3", onlyReal = FALSE), onlyReal = FALSE), "003")
  expect_equal(icd9CondenseShort(icd9ChildrenShort("410", onlyReal = FALSE), onlyReal = FALSE), "410")
  expect_equal(icd9CondenseShort(icd9ChildrenShort("V12", onlyReal = FALSE), onlyReal = FALSE), "V12")
  expect_equal(icd9CondenseShort(icd9ChildrenShort("E800", onlyReal = FALSE), onlyReal = FALSE), "E800")
  # repeat some tests with decimals instead
  expect_equal(icd9CondenseDecimal(icd9Children("003", isShort = FALSE, onlyReal = FALSE), onlyReal = FALSE), "003")
  expect_equal(icd9Condense(icd9ChildrenDecimal("3", onlyReal = FALSE), isShort = FALSE, onlyReal = FALSE), "003")
  expect_equal(icd9CondenseDecimal(icd9ChildrenDecimal("410", onlyReal = FALSE), onlyReal = FALSE), "410")
  expect_equal(icd9CondenseDecimal(icd9Children("V12", isShort = FALSE, onlyReal = FALSE), onlyReal = FALSE), "V12")
  expect_equal(icd9CondenseDecimal(icd9ChildrenDecimal("E800", onlyReal = FALSE), onlyReal = FALSE), "E800")
  # repeat some tests with decimals and smaller codes
  expect_equal(icd9CondenseDecimal(icd9Children("003.2", isShort = FALSE, onlyReal = FALSE), onlyReal = FALSE),
               "003.2")
  expect_equal(icd9Condense(icd9ChildrenDecimal("3.2", onlyReal = FALSE), isShort = FALSE, onlyReal = FALSE),
               "003.2")
  expect_equal(icd9CondenseDecimal(icd9ChildrenDecimal("410.0", onlyReal = FALSE), onlyReal = FALSE), "410.0")
  expect_equal(icd9CondenseDecimal(icd9Children("V12", isShort = FALSE, onlyReal = FALSE), onlyReal = FALSE), "V12")
  expect_equal(icd9CondenseDecimal(icd9ChildrenDecimal("E800", onlyReal = FALSE), onlyReal = FALSE), "E800")

  expect_equal(icd9CondenseShort(icd9ChildrenShort("0031", onlyReal = FALSE), onlyReal = FALSE), "0031")
  # major is allowed
  expect_equal(icd9CondenseShort(c("003", othersalmonella), onlyReal = TRUE), "003")
  # major is returned
  expect_equal(icd9CondenseShort(othersalmonella, onlyReal = TRUE), "003")
  expect_equal(icd9CondenseShort(othersalmonella, onlyReal = FALSE), othersalmonella)
  # now do we find a missing major if all chilren present?
  almostall003 <- icd9ChildrenShort("003", onlyReal = FALSE)
  almostall003 <- almostall003[almostall003 != "003"] # drop the major
  expect_equal(icd9CondenseShort(almostall003, onlyReal = FALSE, toMajor = TRUE), "003")

  # tomajor = false
  expect_equal(icd9CondenseShort(icd9ChildrenShort("0031", onlyReal = FALSE), onlyReal = FALSE, toMajor = FALSE), "0031")
  # major is allowed
  expect_that(res <- icd9CondenseShort(c("003", othersalmonella), onlyReal = TRUE, toMajor = FALSE),
              gives_warning())
  expect_equal(res, character())
  # major is returned
  expect_equal(icd9CondenseShort(othersalmonella, onlyReal = TRUE), "003")
  expect_equal(icd9CondenseShort(othersalmonella, onlyReal = FALSE), othersalmonella)
  # now do we find a missing major if all chilren present?
  almostall003 <- icd9ChildrenShort("003", onlyReal = FALSE)
  almostall003 <- almostall003[almostall003 != "003"] # drop the major
  expect_equal(icd9CondenseShort(almostall003, onlyReal = FALSE, toMajor = TRUE), "003")

})

test_that("condense single major and its children", {
  # message for not specifying onlyReal
  expect_that(res <- icd9CondenseShort("003"), shows_message())
  expect_equal(res, "003")

  skip("TODO: recode these as Explain tests")
  expect_equal(icd9ExplainShort("391"),
               "Rheumatic fever with heart involvement")
  expect_equal(icd9ExplainShort(icd9ChildrenShort("391")),
               "Rheumatic fever with heart involvement")
  expect_equal(icd9ExplainShort(icd9ChildrenShort("391", onlyReal = TRUE)),
               "Rheumatic fever with heart involvement")
})

test_that("condense short range", {

  expect_equal(icd9ExplainShort(icd9Short = othersalmonella),
               "Other salmonella infections")
  expect_equal(icd9ExplainShort(icd9Short = othersalmonella[-3]),
               icd9Hierarchy[c(9, 10, 12:18), "descLong"])

  expect_equal(icd9CondenseToMajorShort(othersalmonella, onlyReal = TRUE), "003")
  expect_equal(icd9CondenseToMajorShort(othersalmonella, onlyReal = FALSE),
               othersalmonella)
  expect_equal(icd9CondenseToMajorShort(othersalmonella[-3], onlyReal = TRUE),
               othersalmonella[-3])
  expect_equal(icd9CondenseToMajorShort(othersalmonella[-3], onlyReal = FALSE),
               othersalmonella[-3])

  expect_equal(sort(icd9ChildrenShort(icd9Short = "001", onlyReal = TRUE)),
               c("0010", "0011", "0019"))

  expect_equal(icd9CondenseShort(icd9ChildrenShort("00320", onlyReal = TRUE), onlyReal = TRUE), "00320")
  # if we ask for real codes, we should expect all real codes as input:
  expect_that(icd9CondenseShort(c("0032", icd9ChildrenShort("0032", onlyReal = TRUE)), onlyReal = TRUE), gives_warning())
  # but majors should be okay, even if not 'real'
  expect_that(icd9CondenseShort(c("003", icd9ChildrenShort("003", onlyReal = TRUE))), testthat::not(gives_warning()))
  # unless we excluded majors:
  expect_that(icd9CondenseShort(c("003", icd9ChildrenShort("003", onlyReal = TRUE)), toMajor = FALSE), shows_message())

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

test_that("working with named lists of codes, decimal is guessed", {
  expect_that(icd9ExplainDecimal(list(a = c("001"), b = c("001.1", "001.9"))), testthat::not(gives_warning()))
  expect_that(icd9Explain(list(a = c("001"), b = c("001.1", "001.9"))), testthat::not(gives_warning()))
})

test_that("icd9 descriptions is parsed correctly", {
  x <- parseIcd9Descriptions()
  expect_equal(names(x), c("icd9", "descLong", "descShort"))
  expect_equal(nrow(x), 14567)
  expect_true(is.character(x$icd9))
  # TODO: add specific tests, e.g. for Menieres with non-standard character
  # sets, punctuation
  # most of the results of this are already tested in icd9Hierarchy
})
