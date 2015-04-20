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

context("icd9Hierarchy was parsed as expected")
# at present, icd9::icd9Hierarchy is derived from RTF parsing, a little web
# scraping, some manually entered data, and (for the short description only)
# another text file parsing.`

test_that("no NA or zero-length values", {
  expect_false(any(sapply(icd9::icd9Hierarchy, is.na)))
  expect_false(any(nchar(unlist(icd9::icd9Hierarchy)) == 0))
})

test_that("factors are in the right place", {
  expect_is(icd9::icd9Hierarchy$icd9, "character")
  expect_is(icd9::icd9Hierarchy$descShort, "character")
  expect_is(icd9::icd9Hierarchy$descLong, "character")
  expect_is(icd9::icd9Hierarchy$threedigit, "factor")
  expect_is(icd9::icd9Hierarchy$major, "factor")
  expect_is(icd9::icd9Hierarchy$subchapter, "factor")
  expect_is(icd9::icd9Hierarchy$chapter, "factor")
})

test_that("codes and descriptions are valid and unique", {
  expect_equal(anyDuplicated(icd9::icd9Hierarchy$icd9), 0)
  expect_true(all(icd9IsValidShort(icd9::icd9Hierarchy$icd9)))
})

test_that("some chapters are correct", {
  chaps <- icd9::icd9Hierarchy$chapter %>% asCharacterNoWarn
  codes <- icd9::icd9Hierarchy$icd9
  # first and last rows (E codes should be last)
  expect_equal(chaps[1], "Infectious And Parasitic Diseases")
  expect_equal(chaps[nrow(icd9::icd9Hierarchy)],
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

test_that("some subchapters are correct", {
  subchaps <- icd9::icd9Hierarchy$subchapter %>% asCharacterNoWarn
  codes <- icd9::icd9Hierarchy$icd9

  # first and last
  expect_equal(subchaps[1], "Intestinal Infectious Diseases")
  expect_equal(subchaps[nrow(icd9::icd9Hierarchy)], "Injury Resulting From Operations Of War")

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
    icd9::icd9Hierarchy[icd9::icd9Hierarchy$icd9 == "5060", ]  %>% sapply(asCharacterNoWarn) %>% unname,
    c("5060", "Fum/vapor bronc/pneumon", "Bronchitis and pneumonitis due to fumes and vapors",
      "506", "Respiratory conditions due to chemical fumes and vapors",
      "Pneumoconioses And Other Lung Diseases Due To External Agents",
      "Diseases Of The Respiratory System")
  )
})

test_that("tricky v91.9 works", {
  expect_equal(
    icd9Hierarchy[icd9Hierarchy$icd9 == "V9192", "descLong"],
    "Other specified multiple gestation, with two or more monoamniotic fetuses")
})
