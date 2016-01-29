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

################################################################################
# ranges
################################################################################

context("test icd10 ranges")

test_that("very bad input data fails completely", {
  expect_error(icd_expand_range(data.frame(a = 1, b = "b"), factor(1,2,3)))
})

test_that("top level ICD-10 ranges of single code are expanded to real codes", {
  expect_true(all(icd_is_defined.icd10cm(icd_expand_range(icd10cm("J11"), icd10cm("J11")))))
 })

test_that("simple ranges of real values works", {
  expect_equal_no_icd(icd_expand_range.icd10cm("A00", "A001"),
               c("A00", "A000", "A001"))
  expect_equal_no_icd(icd_expand_range.icd10cm("A00", "A009", short_code = TRUE),
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
  expect_error(icd_children_defined.icd10cm(data.frame(a = 1, b = "b"), short_code = TRUE))
})

test_that("single value gives correct range", {
  expect_equal(icd_children_defined.icd10cm("A00", short_code = TRUE), c("A00", "A000", "A001", "A009"))
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
