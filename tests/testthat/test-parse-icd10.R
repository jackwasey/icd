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

context("icd10 XML parse")

test_that("icd10 subchapters were parsed correctly", {

  paste("Persons with potential health hazards related",
        "to family and personal history and certain",
        "conditions influencing health status") %>%
    expect_icd10_subchap_equal(start = "Z77", end = "Z99")

  expect_icd10_subchap_equal(
    "Persons encountering health services for examinations",
    "Z00", "Z13")

  expect_icd10_subchap_equal(
    "Occupant of three-wheeled motor vehicle injured in transport accident",
    "V30", "V39")

  expect_icd10_subchap_equal(
    "Malignant neuroendocrine tumors", "C7A", "C7A")

  expect_icd10_subchap_equal(
    "Other human herpesviruses", "B10", "B10")
})

test_that("Y09 got picked up in sub-chapter parsing", {
  # this is actually an error in the 2016 CMS XML which declares a range for
  # Assult from X92-Y08, but has a hanging definition for Y09 with no enclosing
  # chapter. Will have to manually correct for this until fixed.
  expect_icd10_subchap_equal("Assault", "X92", "Y09")

})

