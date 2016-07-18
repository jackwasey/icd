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

context("trim, strim, other utils")

test_that("trim with empty inputs", {
  expect_equal(trim(character(0)), character(0))
  expect_equal(trim(""), "")
})

test_that("trim with acceptable inputs", {
  expect_equal(trim(NA_character_), NA_character_)
  # not necesarily desirable, but this states the expectation.
  expect_equal(trim(NA_integer_), NA_character_)
  expect_equal(trim("jack"), "jack")
  expect_equal(trim("jack "), "jack")
  expect_equal(trim(" jack "), "jack")
  expect_equal(trim(" jack"), "jack")
  expect_equal(trim("jack\t"), "jack")
  expect_equal(trim("\tjack\t"), "jack")
  expect_equal(trim("\tjack"), "jack")
  expect_equal(trim("ja ck"), "ja ck")
  expect_equal(trim("ja ck "), "ja ck")
  expect_equal(trim(" ja ck "), "ja ck")
  expect_equal(trim(" ja ck"), "ja ck")

  expect_equal(trim(c(" ja ck\t", "hayley ")),
               c("ja ck", "hayley"))

  # trim and trimCpp should do the same thing, for non-NA
  expect_identical(trim(c("123", " a s d f ")),
                   trimCpp(c("123", " a s d f ")))

  # strim (trim a single string)
  expect_equal(strim(NA_integer_), NA_character_)
  expect_equal(strim(NA_character_), NA_character_)
  expect_equal(strim("jack"), "jack")
  expect_equal(strim("jack "), "jack")
  expect_equal(strim(" jack "), "jack")
  expect_equal(strim(" jack"), "jack")
  expect_equal(strim("jack\t"), "jack")
  expect_equal(strim("\tjack\t"), "jack")
  expect_equal(strim("\tjack"), "jack")
  expect_equal(strim("ja ck"), "ja ck")
  expect_equal(strim("ja ck "), "ja ck")
  expect_equal(strim(" ja ck "), "ja ck")
  expect_equal(strim(" ja ck"), "ja ck")
  # is this what I want?
  expect_error(strim(c("jack", " an other ")))
  # strim and strimCpp should do the same thing, for non-NA input
  expect_identical(strim(" a s d f "), strimCpp(" a s d f "))

})

test_that("get visit name from a matrix should fail", {
  expect_error(get_visit_name(matrix(c(1, 2, 3, 4), nrow = 2)))
})

test_that("string pair match extraction", {
  expect_error(str_pair_match())
  expect_error(str_pair_match(pattern = "(abc)def(ghi)"))
  expect_error(str_pair_match(string = "bougie"))
  expect_error(str_pair_match(pattern = c("(a)b(c)", "(d)e(f)"),
                            string = "abc"))
  expect_error(str_pair_match(pattern = c("(a)b(c)", "(d)e(f)"),
                            string = c("abc", "def")))

  expect_error(str_pair_match(pattern = "[", string = "abc")) # invalid regex
  # only one parenthesis
  expect_error(str_pair_match(pattern = "(a).*", string = "abc"))
  expect_error(str_pair_match(pattern = ".*(j)", string = "abc"))
  expect_equal(str_pair_match(pattern = "(a*)b(c*)", string = "abc"), c(a = "c"))
  expect_equal(str_pair_match(pattern = "([^mackarel]*)(spain)",
                            string = "togospain"),
               c(togo = "spain"))
  expect_equal(str_pair_match(pattern = "([^mackarel]*)(spain)",
                            string = c("togospain", "djiboutispain")),
               c(togo = "spain", djibouti = "spain"))
  expect_equal(str_pair_match(pattern = "(a*)b(c*)", string = c("abc", "aabcc")),
               c(a = "c", aa = "cc"))
})

test_that("str_pair_match error if more than two outputs", {
  expect_error(str_pair_match(string = "hadoop", pattern = "(ha)(do)(op)"))
  # no error if explicit
  str_pair_match(string = "hadoop", pattern = "(ha)(do)(op)", pos = c(1, 2))
})


test_that("logical to binary for a matrix works", {
  # matrix is all logical or all not logical by definition, so this is easy
  m <- matrix(c(T, F, T, F), nrow = 2)
  n <- matrix(c(1L, 0L, 1L, 0L), nrow = 2)

  expect_identical(logical_to_binary(m), n)

})

test_that("chapter to desc range works for icd9", {
  expect_identical(chapter_to_desc_range.icd9("jack (110-120)"),
    list(Jack = c(start = "110", end = "120"))
  )
  expect_identical(chapter_to_desc_range.icd9("jack (V10-V20)"),
                   list(Jack = c(start = "V10", end = "V20"))
  )
  expect_identical(chapter_to_desc_range.icd9("jack (E990-E991)"),
                   list(Jack = c(start = "E990", end = "E991"))
  )
  expect_identical(chapter_to_desc_range.icd9("jack (110)"),
                   list(Jack = c(start = "110", end = "110"))
  )
  expect_identical(chapter_to_desc_range.icd9("jack (V10)"),
                   list(Jack = c(start = "V10", end = "V10"))
  )
  expect_identical(chapter_to_desc_range.icd9("jack (E990)"),
                   list(Jack = c(start = "E990", end = "E990"))
  )
})

test_that("chapter to desc range works for icd10", {
  expect_identical(chapter_to_desc_range.icd10("jack (A10-A20)"),
                   list(Jack = c(start = "A10", end = "A20"))
  )
  expect_identical(chapter_to_desc_range.icd10("jack (V10-V20)"),
                   list(Jack = c(start = "V10", end = "V20"))
  )
  expect_identical(chapter_to_desc_range.icd10("jack (E99)"),
                   list(Jack = c(start = "E99", end = "E99"))
  )
})

test_that("good random ICD-9 codes are made", {
  rs <- icd9RandomShort(1000);
  expect_true(all(nchar(rs) >= 2))
  expect_true(all(nchar(rs) <= 5))
  # basic regex checks here, not full vaildation
  expect_true(any(grepl(pattern = "^[0-9]{1,5}", rs)))
  expect_true(any(grepl(pattern = "^V.+", rs)))
  expect_true(any(grepl(pattern = "^E.+", rs)))
})
