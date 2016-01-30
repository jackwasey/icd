# Copyright (C) 2014 - 2016  Jack O. Wasey
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

context("icd10 WHO parse")

test_that("icd10 WHO recreated exactly", {
  skip("not implemented yet")
  icd10_get_who_from_cdc()
})

context("icd10 fixed width parse")

test_that("icd10 2016 flat file details are okay", {
  # check cols at a time, so I get better error feedback:
  col_names <- c("code", "billable", "descShort", "descLong", "threedigit",
              "major", "subchapter", "chapter")
  expect_warning(res <- icd10cm_get_all_defined(save_data = FALSE), NA)
  expect_identical(colnames(res), col_names)

  checkmate::expect_character(res$code, any.missing = FALSE)
  checkmate::expect_logical(res$billable, any.missing = FALSE)
  checkmate::expect_character(res$descShort, any.missing = FALSE)
  checkmate::expect_character(res$descLong, any.missing = FALSE)

  checkmate::expect_character(icd9::icd10cm2016$code, any.missing = FALSE)
  checkmate::expect_logical(icd9::icd10cm2016$billable, any.missing = FALSE)
  checkmate::expect_character(icd9::icd10cm2016$descShort, any.missing = FALSE)
  checkmate::expect_character(icd9::icd10cm2016$descLong, any.missing = FALSE)

  for (n in c("threedigit", "major", "subchapter", "chapter")) {
      #checkmate::expect_factor(res[[n]], empty.levels.ok = FALSE, any.missing = FALSE)
      #checkmate::expect_factor(icd9::icd10cm2016[[n]], empty.levels.ok = FALSE, any.missing = FALSE)
      expect_identical(levels(res[[n]]), levels(icd9::icd10cm2016[[n]]))
      expect_identical(res[[n]], icd9::icd10cm2016[[n]], info = paste("working on ", n))
  }
  expect_identical(res, icd9::icd10cm2016)
})

context("icd10 XML parse")

test_that("icd10 sub-chapters are recreated exactly", {
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

test_that("ICD-10 chapters and subchapters are distinct", {
  # and for good measure, make sure that sub-chapters and chapters are not
  # confused. This was really just a problem with RTF parsing for ICD-9, but
  # there are possible similiar problems with some of the XML hierarchy.

  for (chap in names(icd9::icd10_chapters))
    expect_icd10_only_chap(chap)

  for (subchap in names(icd9::icd10_sub_chapters))
    expect_icd10_only_sub_chap(subchap)
})

test_that("Y09 got picked up in sub-chapter parsing", {
  # this is actually an error in the 2016 CMS XML which declares a range for
  # Assult from X92-Y08, but has a hanging definition for Y09 with no enclosing
  # chapter. Will have to manually correct for this until fixed.
  expect_icd10_sub_chap_equal("Assault", "X92", "Y09")
})

