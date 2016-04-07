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

test_icd10_most_majors <- outer(LETTERS, sprintf(0:99, fmt = "%02i"), paste0)

context("icd10 WHO parse")

test_that("icd10 WHO recreated exactly", {
  skip("not implemented yet")
})

context("icd10 fixed width parse")

test_that("icd10 2016 flat file details are okay", {

  skip_icd10cm_flat_avail()

  # check cols at a time, so I get better error feedback:
  col_names <- c("code", "billable", "short_desc", "long_desc", "three_digit",
              "major", "sub_chapter", "chapter")
  expect_warning(res <- icd10cm_get_all_defined(save_data = FALSE), regexp = NA)
  expect_identical(colnames(res), col_names)

  # checkmate tests worked well here, but don't work with latest testthat
  expect_true(is.character(res$code))
  expect_true(is.logical(res$billable))
  expect_true(is.character(res$short_desc))
  expect_true(is.character(res$long_desc))

  expect_true(is.character(icd::icd10cm2016$code))
  expect_true(is.logical(icd::icd10cm2016$billable))
  expect_true(is.character(icd::icd10cm2016$short_desc))
  expect_true(is.character(icd::icd10cm2016$long_desc))

  for (n in c("three_digit", "major", "sub_chapter", "chapter")) {
      expect_true(is.factor(res[[n]]))
      expect_true(is.factor(icd::icd10cm2016[[n]]))
      expect_identical(levels(res[[n]]), levels(icd::icd10cm2016[[n]]), info = paste("working on ", n))
      expect_identical(res[[n]], icd::icd10cm2016[[n]], info = paste("working on ", n))
  }
  expect_identical(res, icd::icd10cm2016)
})

context("icd10 XML parse")

test_that("icd10 sub-chapters are recreated exactly", {
  skip_icd10cm_xml_avail()
  expect_identical(
    icd10cm_extract_sub_chapters(save_data = FALSE),
    icd10_sub_chapters
  )
})

test_that("icd10 sub_chapters were parsed correctly", {

  paste("Persons with potential health hazards related",
        "to family and personal history and certain",
        "conditions influencing health status") %>%
    expect_icd10_sub_chap_equal(start = "Z77", end = "Z99")

  expect_icd10_sub_chap_equal(
    "Persons encountering health services for examinations",
    "Z00", "Z13")

  expect_icd10_sub_chap_equal(
    "Occupant of three-wheeled motor vehicle injured in transport accident",
    "V30", "V39")

  expect_icd10_sub_chap_equal(
    "Malignant neuroendocrine tumors", "C7A", "C7A")

  expect_icd10_sub_chap_equal(
    "Other human herpesviruses", "B10", "B10")
})

test_that("ICD-10 chapters and sub-chapters are distinct", {
  # and for good measure, make sure that sub-chapters and chapters are not
  # confused. This was really just a problem with RTF parsing for ICD-9, but
  # there are possible similiar problems with some of the XML hierarchy.

  for (chap in names(icd::icd10_chapters))
    expect_icd10_only_chap(chap)

  for (subchap in names(icd::icd10_sub_chapters))
    expect_icd10_only_sub_chap(subchap)
})

test_that("Y09 got picked up in sub-chapter parsing", {
  # this is actually an error in the 2016 CMS XML which declares a range for
  # Assult from X92-Y08, but has a hanging definition for Y09 with no enclosing
  # chapter. Will have to manually correct for this until fixed.
  expect_icd10_sub_chap_equal("Assault", "X92", "Y09")
})
