# Copyright (C) 2014 - 2015  Jack O. Wasey
#
# This file is part of icd9.
#
# icd9 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd9 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd9. If not, see <http:#www.gnu.org/licenses/>.

# context("icd10 ranges")
#
# test_that("top level ICD-10 ranges between different alphabetic groups are not valid", {
#   # is this valid? injury codes, for example, go through S and T.
#   expect_error(icd10ExpandRange("A10", "B20"))
#   expect_error(icd10ExpandRange("C99", "B00"))
#   expect_error(icd10ExpandRange("G99.x", "H01"))
#   expect_error(icd10ExpandRange("G99", "H01.x"))
#   expect_error(icd10ExpandRange("G99.x", "H01.x"))
# })

# test_that("top level ICD-10 ranges of single code are expanded to real codes", {
#   #expect_equals(icd10ExpandRange("J11", "J11", onlyReal = TRUE), )
# })

################################################################################
# ranges
################################################################################

context("test icd10 ranges")

test_that("very bad input data fails completely", {
  expect_error(icd_expand_range(data.frame(a = 1, b = "b"), factor(1,2,3)))
})

test_that("simple ranges of real values works", {
  expect_equal(icd_expand_range.icd10cm("A00", "A001"),
               c("A00", "A000", "A001"))
  expect_equal(icd_expand_range.icd10cm("A00", "A009", short_code = TRUE),
               c("A00", "A000", "A001", "A009"))
})

test_that("range style used by Quan is accepted", {
  # quan_ranges <- list(
  #   c("M31.0", "M31.3"),
  #   c("M32.x", "M35.x"),
  #   c("D65", "D68.x"),
  #   c("M45.x", "M45.x"),
  #   c("M46.1", "M46.1")
  # )
  quan_ranges <- list(
    c("M310", "M313"),
    c("M32x", "M35x"),
    c("D65", "D68x"),
    c("M45x", "M45x"),
    c("M461", "M461")
  )
  for (pair in quan_ranges) {
    # start with a low bar!
      expect_error(
        icd_expand_range.icd10cm(pair[1], pair[2], short_code = TRUE), NA,
        info = paste(pair, collapse = "-")
        )

    # the input values should be in the output? NOT necessarily true for .x numbers
    # expect_true(
    #   all(pair %in% icd10ExpandRangeRealShort(pair[1], pair[2])),
    #   info = paste(pair, collapse = "-")
    # )

  }

})

################################################################################
# ranges for real children
################################################################################

context("test icd10 ranges - children real")

test_that("completely invalid input fails", {
  expect_error(icd_children_real.icd10cm(data.frame(a = 1, b = "b"), short_code = TRUE))
})

test_that("single value gives correct range", {
  expect_equal(icd_children_real.icd10cm("A00", short_code = TRUE), c("A00", "A000", "A001", "A009"))
})

################################################################################
# ranges for (some) possible children - low priority
################################################################################

context("test icd10 ranges - children possible")

test_that("completely invalid input fails", {

  expect_error(icd10ChildrenPossibleShort(data.frame(a = 1, b = "b")))

})

test_that("single values give all real children", {
  expect_equal(icd_children_real.icd10cm("A00", short_code = TRUE), c("A00", "A000", "A001", "A009"))
})

test_that("certainly invalid codes return empty vectors", {
  expect_equal(icd10ChildrenPossibleShort("!"), character(0))
  expect_equal(icd10ChildrenPossibleShort("H"), character(0))
  expect_equal(icd10ChildrenPossibleShort("H7"), character(0))
  # never longer than 7 digits no matter what
  expect_equal(icd10ChildrenPossibleShort("12345678"), character(0))
  expect_equal(icd10ChildrenPossibleShort("A2345678"), character(0))
})

test_that("NA values are handled gracefully", {
  expect_equal(icd10ChildrenPossibleShort(NA_character_), NA_character_)
  expect_equal(icd10ChildrenPossibleShort(c(NA_character_, NA_character_)),
               c(NA_character_, NA_character_))
})

test_that("exact children created for single values", {
  expect_equal(icd10ChildrenPossibleShort("A234567"), "A234567")

  expect_equal(
    icd10ChildrenPossibleShort("123456"),
    c("123456", "1234560", "1234561", "1234562", "1234563", "1234564",
      "1234565", "1234569", "123456A", "123456B", "123456C", "123456D",
      "123456E", "123456F", "123456G", "123456H", "123456J", "123456K",
      "123456M", "123456N", "123456P", "123456Q", "123456R", "123456S"))
})

test_that("right number of children are made for higher level codes", {
  #sapply(minor_chars, length)  [1] 23 14 11 23 (all +1)
  expect_equal(length(icd10ChildrenPossibleShort("Z99")), 24 * 15 * 12 * 24)
  expect_equal(length(icd10ChildrenPossibleShort("Z991")), 15 * 12 * 24)
  expect_equal(length(icd10ChildrenPossibleShort("Z9921")), 12 * 24)
  expect_equal(length(icd10ChildrenPossibleShort("Z99321")), 24)
})

test_that("multiple inputs return ordered results", {
  expect_equal(
    icd10ChildrenPossibleShort(c("A00", "Z99")),
    c(icd10ChildrenPossibleShort("A00"), icd10ChildrenPossibleShort("Z99"))
  )

  expect_equal(
    icd10ChildrenPossibleShort(c("Z99", "A00")),
    c(icd10ChildrenPossibleShort("A00"), icd10ChildrenPossibleShort("Z99"))
  )
})


# to move somewhere else:
test_that("ICD-10 codes in uranium data are okay", {
  skip("reinstate this test once ICD-10 WHO codes are available for comparison. Uranium Pathology data is not ICD-10-CM, but ICD-10 WHO.")
  expect_true(
    all(strip(uranium_pathology$icd10, ".") %in% icd10cm2016$code)
  )
  # codes missing from RHS:
  setdiff(uranium_pathology$icd10  %>%  strip("."), icd10cm2016$code)

  # http://apps.who.int/classifications/icd10/browse/2015/en#!/Y86

})
