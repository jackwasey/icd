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

context("explain ICD-9: code to human-readable")

test_that("explain a large set of ICD-9 codes succinctly", {
  expect_identical(
    icd_explain(icd_children.icd9("391", defined = FALSE, short_code = TRUE),
                condense = FALSE, short_code = TRUE),
    c("Rheumatic fever with heart involvement", "Acute rheumatic pericarditis",
      "Acute rheumatic endocarditis", "Acute rheumatic myocarditis",
      "Other acute rheumatic heart disease",
      "Acute rheumatic heart disease, unspecified")
  )

  expect_identical(
    icd_explain.icd9(icd_children.icd9(as.icd_short_diag("391")), condense = TRUE, short_code = TRUE),
    "Rheumatic fever with heart involvement"
  )
})

test_that("explain a large set of ICD-9 codes succinctly, using factors", {
  expect_identical(
    icd_explain(factor(icd_children.icd9("391", defined = FALSE, short_code = TRUE)),
                condense = FALSE, short_code = TRUE),
    c("Rheumatic fever with heart involvement", "Acute rheumatic pericarditis",
      "Acute rheumatic endocarditis", "Acute rheumatic myocarditis",
      "Other acute rheumatic heart disease",
      "Acute rheumatic heart disease, unspecified")
  )

  expect_identical(
    icd_explain.icd9(factor(icd_children.icd9(as.icd_short_diag("391"))), condense = TRUE, short_code = TRUE),
    "Rheumatic fever with heart involvement"
  )
})

test_that("github issue #41", {
  expect_equal(
    icd_explain.icd9(icd_get_defined.icd9(icd9_map_quan_deyo[["Dementia"]]), condense = TRUE),
    icd_explain.icd9(icd9_map_quan_deyo[["Dementia"]], condense = TRUE, warn = FALSE)
  )
  expect_equal(
    icd_explain.icd9(icd_get_defined(icd9_map_quan_deyo[["Dementia"]]), condense = FALSE),
    icd_explain.icd9(icd9_map_quan_deyo[["Dementia"]], condense = FALSE)
  )
})

test_that("explain S3 dispatch", {
  expect_equal(icd_explain.icd9("003.21", short_code = FALSE), "Salmonella meningitis")
  expect_equal(icd_explain("003.21", short_code = FALSE),
               icd_explain.icd9("003.21", short_code = FALSE))
  expect_equal(icd_explain.list(list(a = "003.21"), short_code = FALSE),
               list(a = icd_explain.icd9("00321", short_code = TRUE)))
  expect_equal(icd_explain.list(list(a = "003.21", b = "390"), short_code = FALSE),
               list(a = icd_explain.icd9cm("00321", short_code = TRUE),
                    b = "Rheumatic fever without mention of heart involvement"))
  expect_warning(res <- icd_explain.icd9(c(a = "not", b = "icd9code"), short_code = TRUE))
  expect_equal(res, character(0))
  expect_warning(res <- icd_explain(list(a = icd9("not"), b = icd9("icd9code")), short_code = FALSE))
  expect_equal(res, list(a = character(0), b = character(0)))

  expect_equal(icd_explain.icd9("00321", short_code = TRUE), "Salmonella meningitis")

})

test_that("explain single top level code which is billable, has no children", {
  # the code "390" is a billable major: good test case.
  expect_identical(icd_explain.icd9("390"),
                   "Rheumatic fever without mention of heart involvement")
})

test_that("expalin a single top level code without a top level explanation", {
  expect_identical(icd_explain("391"),
                   "Rheumatic fever with heart involvement")
})


test_that("explain a single leaf node", {
  expect_equal(icd_explain.icd9("27800", condense = FALSE), "Obesity, unspecified")
  expect_equal(icd_explain.icd9("27800", condense = TRUE), "Obesity, unspecified")
  expect_equal(icd_explain.icd9("00329"), "Other localized salmonella infections")
})

test_that("explain handles mix of valid and invalid", {
  expect_equal(icd_explain.icd9(c("radishes", "123"), warn = FALSE), "Other cestode infection")
  expect_warning(icd_explain.icd9(c("radishes", "123")))
})

test_that("explain works when none ICD-9 codes are even valid", {
  expect_equal(icd_explain.icd9(c("radishes", "123123", NA), warn = FALSE), character(0))
})


test_that("guess icd9 types: short", {
  expect_true(icd_guess_short("12345"))
  expect_true(icd_guess_short(c("12345", "234")))
  # we only look at first one...
  expect_true(icd_guess_short(c("12345", "23.4")))
  expect_true(icd_guess_short("1234"))
})

test_that("guess icd9 types: decimal", {
  expect_false(icd_guess_short("123.45"))
  expect_false(icd_guess_short("123.4"))
  expect_false(icd_guess_short("123."))
})

test_that("guess icd9 types: invalid", {
  expect_equal(icd_guess_short(NA_character_), TRUE)
})

test_that("guess with just majors", {
  # it acutally doesn't matter if they are all majors, so we default to 'short'
  # which is usually the most direct route to an answer
  expect_true(icd_guess_short(c("100", "101", "102")))
})

test_that("extract top-level codes from the RTF gives the complete list", {
  # total number of codes
  # all valid major form.

  # format of each hierarchy level:
  expect_equal(names(icd9_chapters[[1]]), c("start", "end"))
  expect_equal(names(icd9_sub_chapters[[1]]), c("start", "end"))
  # should be no NA values
  expect_true(all(!is.na(vapply(icd9_chapters, "[[", FUN.VALUE = "", 1))))
  expect_true(all(!is.na(vapply(icd9_chapters, "[[", FUN.VALUE = "", 2))))
  expect_true(all(!is.na(vapply(icd9_sub_chapters, "[[", FUN.VALUE = "", 1))))
  expect_true(all(!is.na(vapply(icd9_sub_chapters, "[[", FUN.VALUE = "", 2))))
  expect_true(all(!is.na(vapply(icd9_majors, "[[", FUN.VALUE = "", 1))))

  # all the range limits and majors should be valid majors
  expect_true(
    all(icd_is_valid_major.icd9(vapply(icd9_chapters, "[[", FUN.VALUE = "", 1))))
  expect_true(
    all(icd_is_valid_major.icd9(vapply(icd9_chapters, "[[", FUN.VALUE = "", 2))))
  expect_true(
    all(icd_is_valid_major.icd9(vapply(icd9_sub_chapters, "[[", FUN.VALUE = "", 1))))
  expect_true(
    all(icd_is_valid_major.icd9(vapply(icd9_sub_chapters, "[[", FUN.VALUE = "", 2))))
  expect_true(
    all(icd_is_valid_major.icd9(vapply(icd9_majors, "[[", FUN.VALUE = "", 1))))
})

test_that("icd9_majors - positive values", {
  expect_true("001" %in% icd9_majors)
  expect_true("V01" %in% icd9_majors)
  expect_true("V91" %in% icd9_majors) # only up to V89 in 2011.
  expect_true("E000" %in% icd9_majors)
  expect_true("E001" %in% icd9_majors)
  expect_true("E009" %in% icd9_majors)
  expect_true("E013" %in% icd9_majors)
  expect_true("E016" %in% icd9_majors)
  expect_true("E849" %in% icd9_majors)
  expect_true("E917" %in% icd9_majors)
})

test_that("icd9_majors - negative values", {
  # there are some gaps: just make sure we don't have any spurious codes:
  expect_false("888" %in% icd9_majors)
  expect_false("889" %in% icd9_majors)
  expect_false("V00" %in% icd9_majors)
  expect_false("V38" %in% icd9_majors)
  expect_false("E777" %in% icd9_majors)
})

for (i in list("icd9_chapters", "icd9_sub_chapters", "icd9_majors")) {
  il <- get(i)
  test_that(paste("icd9Chapters... duplication, blanks:", i), {
    # should be no duplicate names
    expect_equal(anyDuplicated(names(il)), 0)
    expect_equal(length(il), length(unique(names(il))))
    # or duplicate codes
    expect_equal(anyDuplicated(il), 0)
    expect_equal(length(il), length(unique(il)))

    expect_true(all(nchar(names(il) > 0)))
    expect_false(anyNA(il))
    expect_false(anyNA(names(il)))
  })
}

test_that("parse icd9_majors vs those listed
          in the other CDC source of the leaf definitions.", {
            # get all the majors from the other list, to compare

            compare_majors <- unique(icd_get_major.icd9(icd::icd9cm_hierarchy[["code"]], short_code = TRUE))
            expect_true(all(compare_majors %in% icd9_majors))
            expect_true(all(icd9_majors %in% compare_majors))
          })

test_that("unsorted hierarchy tests", {
  expect_equal(
    tolower(icd::icd9cm_hierarchy[icd9cm_hierarchy[["code"]] == "00321",
                                  "long_desc"]),
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
  chaps1 <- icd9_get_chapters(c("410", "411", "412"), short_code = TRUE)
  expect_equal(nrow(chaps1), 3)

  chaps2 <- icd9_get_chapters("418", short_code = TRUE)
  expect_is(chaps2, "data.frame")
  expect_is(chaps2$three_digit, "factor")
  expect_is(chaps2$major, "factor")
  expect_is(chaps2$sub_chapter, "factor")
  expect_is(chaps2$chapter, "factor")
  expect_equal(as_char_no_warn(chaps2$three_digit), NA_character_)
  expect_equal(as_char_no_warn(chaps2$major), NA_character_)
  expect_equal(as_char_no_warn(chaps2$sub_chapter), NA_character_)
  expect_equal(as_char_no_warn(chaps2$chapter), NA_character_)

  chaps3 <- icd9_get_chapters("417", short_code = FALSE)
  expect_equal(as_char_no_warn(chaps3$three_digit), "417")
  expect_equal(as_char_no_warn(chaps3$major),
               "Other diseases of pulmonary circulation")
  expect_equal(as_char_no_warn(chaps3$sub_chapter),
               "Diseases Of Pulmonary Circulation")
  expect_equal(as_char_no_warn(chaps3$chapter),
               "Diseases Of The Circulatory System")

  chaps4 <- icd9_get_chapters("417", short_code = TRUE)
  chaps5 <- icd9_get_chapters("417.1", short_code = FALSE)
  chaps6 <- icd9_get_chapters("4171", short_code = TRUE)
  chaps7 <- icd9_get_chapters("417.1", short_code = FALSE)
  chaps8 <- icd9_get_chapters("4171", short_code = TRUE)
  expect_equal(chaps3, chaps4)
  expect_equal(chaps3, chaps5)
  expect_equal(chaps3, chaps6)
  expect_equal(chaps3, chaps7)
  expect_equal(chaps3, chaps8)

})

test_that("working with named lists of codes, decimal is guessed", {
  expect_warning(icd_explain(
    list(a = c("001"), b = c("001.1", "001.9")), short_code = FALSE), regexp = NA)
  expect_warning(icd_explain(list(a = c("001"), b = c("001.1", "001.9"))), regexp = NA)
})

test_that("icd9 descriptions is parsed correctly", {
  skip_flat_icd9_avail("32")
  x <- icd9_parse_leaf_desc_ver(version = "32", offline = TRUE)
  expect_equal(names(x), c("code", "short_desc", "long_desc"))
  expect_equal(nrow(x), 14567)
  expect_true(is.character(x$code))
})
