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

context("icd9cm_hierarchy was parsed as expected")
# at present, icd::icd9cm_hierarchy is derived from RTF parsing, a little web
# scraping, some manually entered data, and (for the short description only)
# another text file parsing.`

test_that("no NA or zero-length values", {
  expect_false(any(sapply(icd::icd9cm_hierarchy, is.na)))
  expect_false(any(nchar(unlist(icd::icd9cm_hierarchy)) == 0))
})

test_that("factors are in the right place", {
  expect_is(icd::icd9cm_hierarchy[["code"]], c("icd9cm", "icd9", "character"))
  expect_is(icd::icd9cm_hierarchy$short_desc, "character")
  expect_is(icd::icd9cm_hierarchy$long_desc, "character")
  expect_is(icd::icd9cm_hierarchy$three_digit, "factor")
  expect_is(icd::icd9cm_hierarchy$major, "factor")
  expect_is(icd::icd9cm_hierarchy$sub_chapter, "factor")
  expect_is(icd::icd9cm_hierarchy$chapter, "factor")
})

test_that("codes and descriptions are valid and unique", {
  expect_equal(anyDuplicated(icd::icd9cm_hierarchy[["code"]]), 0)
  expect_true(all(icd9IsValidShort(icd::icd9cm_hierarchy[["code"]])))
})

test_that("some chapters are correct", {
  chaps <- icd::icd9cm_hierarchy$chapter %>% as_char_no_warn
  codes <- icd::icd9cm_hierarchy[["code"]]
  # first and last rows (E codes should be last)
  expect_equal(chaps[1], "Infectious And Parasitic Diseases")
  expect_equal(chaps[nrow(icd::icd9cm_hierarchy)],
               "Supplementary Classification Of External Causes Of Injury And Poisoning")

  # first and last rows of a block in the middle
  neoplasm_first_row <- which(codes == "140")
  neoplasm_last_row <- which(codes == "240") - 1
  expect_equal(chaps[neoplasm_first_row - 1], "Infectious And Parasitic Diseases")
  expect_equal(chaps[neoplasm_first_row], "Neoplasms")
  expect_equal(chaps[neoplasm_last_row], "Neoplasms")
  expect_equal(chaps[neoplasm_last_row + 1],
               "Endocrine, Nutritional And Metabolic Diseases, And Immunity Disorders")
})

test_that("some sub-chapters are correct", {
  subchaps <- icd::icd9cm_hierarchy$sub_chapter %>% as_char_no_warn
  codes <- icd::icd9cm_hierarchy[["code"]]

  # first and last
  expect_equal(subchaps[1], "Intestinal Infectious Diseases")
  expect_equal(subchaps[nrow(icd::icd9cm_hierarchy)], "Injury Resulting From Operations Of War")

  # first and last of a block in the middle
  suicide_rows <- which(codes %in% ("E950" %i9sa% "E959"))
  expect_equal(subchaps[suicide_rows[1] - 1],
               "Drugs, Medicinal And Biological Substances Causing Adverse Effects In Therapeutic Use")
  expect_equal(subchaps[suicide_rows[1]], "Suicide And Self-Inflicted Injury")
  expect_equal(subchaps[suicide_rows[length(suicide_rows)]], "Suicide And Self-Inflicted Injury")
  expect_equal(subchaps[suicide_rows[length(suicide_rows)] + 1],
               "Homicide And Injury Purposely Inflicted By Other Persons")
})

test_that("some randomly selected rows are correct", {
  expect_equal(
    icd::icd9cm_hierarchy[icd::icd9cm_hierarchy[["code"]] == "5060", ]  %>% sapply(as_char_no_warn) %>% unname,
    c("5060", "Fum/vapor bronc/pneumon", "Bronchitis and pneumonitis due to fumes and vapors",
      "506", "Respiratory conditions due to chemical fumes and vapors",
      "Pneumoconioses And Other Lung Diseases Due To External Agents",
      "Diseases Of The Respiratory System")
  )
})

test_that("tricky v91.9 works", {
  expect_equal(
    icd9cm_hierarchy[icd9cm_hierarchy[["code"]] == "V9192", "long_desc"],
    "Other specified multiple gestation, with two or more monoamniotic fetuses")
})
