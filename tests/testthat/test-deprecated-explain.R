# Copyright (C) 2014 - 2016  Jack O. Wasey
#
# This file is part of icd.
#
# icd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd. If not, see <http:#www.gnu.org/licenses/>.

context("deprecated explain ICD-9: code to human-readable")

test_that("explain a large set of ICD-9 codes succinctly", {
  expect_identical(
    icd9ExplainShort(icd9ChildrenShort("391", onlyReal = FALSE),
                     doCondense = FALSE),
    c("Rheumatic fever with heart involvement", "Acute rheumatic pericarditis",
      "Acute rheumatic endocarditis", "Acute rheumatic myocarditis",
      "Other acute rheumatic heart disease",
      "Acute rheumatic heart disease, unspecified")
  )

  expect_identical(
    icd9ExplainShort(icd9ChildrenShort("391"), doCondense = TRUE),
    "Rheumatic fever with heart involvement"
  )

})

test_that("github issue #41", {
  expect_equal(
    icd9Explain(icd9GetReal(quanDeyoComorbid[["Dementia"]]), doCondense = TRUE),
    icd9Explain(quanDeyoComorbid[["Dementia"]], doCondense = TRUE, warn = FALSE)
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
               list(a = icd9Explain("00321", isShort = TRUE)))
  expect_equal(icd9Explain.list(list(a = "003.21", b = "390"), isShort = FALSE),
               list(a = icd9Explain("00321", isShort = TRUE),
                    b = "Rheumatic fever without mention of heart involvement"))
  expect_warning(res <- icd9Explain(list(a = "not", b = "icd9code"), isShort = TRUE))
  expect_equal(res, list(a = character(0), b = character(0)))
  expect_warning(res <- icd9Explain(list(a = "not", b = "icd9code"), isShort = FALSE))
  expect_equal(res, list(a = character(0), b = character(0)))
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
  expect_equal(icd9ExplainShort("27800", doCondense = FALSE), "Obesity, unspecified")
  expect_equal(icd9ExplainShort("27800", doCondense = TRUE), "Obesity, unspecified")
  expect_equal(icd9Explain("00329"), "Other localized salmonella infections")
})

test_that("explain handles mix of valid and invalid", {
  expect_equal(icd9Explain(c("radishes", "123"), warn = FALSE), "Other cestode infection")
  expect_warning(icd9Explain(c("radishes", "123")))
})

test_that("explain works when none ICD-9 codes are even valid", {
  expect_equal(icd9Explain(c("radishes", "123123", NA), warn = FALSE), character(0))
})


test_that("guess icd9 types: short", {
  expect_true(icd9GuessIsShort("12345"))
  expect_true(icd9GuessIsShort(c("12345", "234")))
  # we only look at first one...
  expect_true(icd9GuessIsShort(c("12345", "23.4")))
  expect_true(icd9GuessIsShort("1234"))
})

test_that("guess icd9 types: ambiguous, default to short", {
  skip("this is no longer expected functionality")
  expect_equal(icd9GuessIsShort(c("123.4", "2345")), TRUE)
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

  # format of each hierarchy level:
  expect_equal(names(icd9Chapters[[1]]), c("start", "end"))
  expect_equal(names(icd9ChaptersSub[[1]]), c("start", "end"))
  expect_equal(names(icd9ChaptersMajor[[1]]), c("major"))
  # should be no NA values
  expect_true(all(!is.na(vapply(icd9Chapters, "[[", FUN.VALUE = "", 1))))
  expect_true(all(!is.na(vapply(icd9Chapters, "[[", FUN.VALUE = "", 2))))
  expect_true(all(!is.na(vapply(icd9ChaptersSub, "[[", FUN.VALUE = "", 1))))
  expect_true(all(!is.na(vapply(icd9ChaptersSub, "[[", FUN.VALUE = "", 2))))
  expect_true(all(!is.na(vapply(icd9ChaptersMajor, "[[", FUN.VALUE = "", 1))))

  # all the range limits and majors should be valid majors
  expect_true(
    all(icd9IsValidMajor(vapply(icd9Chapters, "[[", FUN.VALUE = "", 1))))
  expect_true(
    all(icd9IsValidMajor(vapply(icd9Chapters, "[[", FUN.VALUE = "", 2))))
  expect_true(
    all(icd9IsValidMajor(vapply(icd9ChaptersSub, "[[", FUN.VALUE = "", 1))))
  expect_true(
    all(icd9IsValidMajor(vapply(icd9ChaptersSub, "[[", FUN.VALUE = "", 2))))
  expect_true(
    all(icd9IsValidMajor(vapply(icd9ChaptersMajor, "[[", FUN.VALUE = "", 1))))
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
    expect_false(anyNA(il))
    expect_false(anyNA(names(il)))
  })
}

test_that("parse icd9ChaptersMajor vs those listed
          in the other CDC source of the leaf definitions.", {
            # get all the majors from the other list, to compare

            compareMajors <- unique(icd9GetMajor(icd::icd9Hierarchy$icd9,
                                                 isShort = TRUE))
            expect_true(all(compareMajors %in% icd9ChaptersMajor))
            expect_true(all(icd9ChaptersMajor %in% compareMajors))
          })

test_that("unsorted hierarchy tests", {
  expect_equal(
    tolower(icd::icd9Hierarchy[icd9Hierarchy[["icd9"]] == "00321",
                               "descLong"]),
    tolower("Salmonella Meningitis"))
})

test_that("explain gives appropriate warnings by default", {
  # if we ask for real codes, we should expect all real codes as input:
  expect_that(icd9CondenseShort("E7777", onlyReal = TRUE, warn = TRUE), gives_warning())
})

test_that("explain icd9GetChapters bad input", {
  skip("todo")
})

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

})

test_that("working with named lists of codes, decimal is guessed", {
  # warnings given for deprecated code
  icd9ExplainDecimal(list(a = c("001"), b = c("001.1", "001.9")))
  icd9Explain(list(a = c("001"), b = c("001.1", "001.9")))
})

test_that("icd9 descriptions was parsed correctly", {
  # TODO: only skip online if file not already available.
  x <- icd9Billable[["32"]]
  expect_equal(names(x), c("icd9", "descShort", "descLong"))
  expect_equal(nrow(x), 14567)
  expect_true(is.character(x$icd9))
})
