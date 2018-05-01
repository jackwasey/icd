# Copyright (C) 2014 - 2018  Jack O. Wasey
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

context("ICD-10 codes are valid, defined, billable")

test_that("icd10 codes that are billable and do exist", {
  test_codes <- list("A000", "A001", "A009", "A09", "R197", "B0059",
                     "C7A024", "C7B1", "D4989", "O9A119", "O9A519", "O9A53", "O99845", "P969",
                     "V99XXXS", "V99XXXD", "V99XXXA", "V988XXS",
                     "W009XXA", "Y95", "Y999",
                     "Z0000")
  test_codes <- append(test_codes, lapply(test_codes, icd10cm))
  for (x in test_codes) {
    expect_true(is_valid(x), info = x)
    expect_true(is_valid(x, short_code = TRUE), info = x)
    expect_true(is_valid.icd10(x), info = x)
    expect_true(is_valid.icd10(x, short_code = TRUE), info = x)
    # expect_true(is_valid.icd10cm(x)
    # expect_true(is_valid.icd10cm(x, short_code = TRUE)
    expect_true(is_billable(x), info = x)
    expect_true(is_billable(x, short_code = TRUE), info = x)
    expect_true(is_billable.icd10(x), info = x)
    expect_true(is_billable.icd10(x, short_code = TRUE), info = x)
    expect_true(is_billable.icd10cm(x), info = x)
    expect_true(is_billable.icd10cm(x, short_code = TRUE), info = x)
    expect_true(is_defined(x), info = x)
    expect_true(is_defined(x, short_code = TRUE), info = x)
    expect_true(is_defined.icd10(x), info = x)
    expect_true(is_defined.icd10(x, short_code = TRUE), info = x)
    expect_true(is_defined.icd10cm(x), info = x)
    expect_true(is_defined.icd10cm(x, short_code = TRUE), info = x)
  }
})

test_that("icd10 codes that are not billable but do exist", {
  test_codes <- list("A00", "A01", "R19", "B005",
                     "C7A02", "C7B", "D498", "O9A11", "O9A51", "O9A5", "O9984", "P96",
                     # SOMEDAY might consider stripping trailing X, but doubt this occur in real life:
                     # "V99XXX", "V988XX", "W009XX",
                     "Y99", "Z000")
  test_codes <- append(test_codes, lapply(test_codes, icd10cm))
  for (x in test_codes) {
    expect_true(is_valid(x), info = x)
    expect_true(is_valid(x, short_code = TRUE), info = x)
    expect_true(is_valid.icd10(x), info = x)
    expect_true(is_valid.icd10(x, short_code = TRUE), info = x)
    # expect_true(is_valid.icd10cm(x)
    # expect_true(is_valid.icd10cm(x, short_code = TRUE)
    expect_false(is_billable(x), info = x)
    expect_false(is_billable(x, short_code = TRUE), info = x)
    expect_false(is_billable.icd10(x), info = x)
    expect_false(is_billable.icd10(x, short_code = TRUE), info = x)
    expect_false(is_billable.icd10cm(x), info = x)
    expect_false(is_billable.icd10cm(x, short_code = TRUE), info = x)
    expect_true(is_defined(x), info = x)
    expect_true(is_defined(x, short_code = TRUE), info = x)
    expect_true(is_defined.icd10(x), info = x)
    expect_true(is_defined.icd10(x, short_code = TRUE), info = x)
    expect_true(is_defined.icd10cm(x), info = x)
    expect_true(is_defined.icd10cm(x, short_code = TRUE), info = x)
  }
})

test_that("major dispatch intact", {
  expect_true(is_valid_major(icd9("V10")))
  expect_true(is_valid_major(as.icd9cm("V10")))
  expect_true(is_valid_major(icd10("V10")))
  expect_true(is_valid_major(as.icd10cm("V10")))
  expect_false(is_valid_major(icd9("A12")))
  expect_false(is_valid_major(as.icd9cm("A12")))
  expect_false(is_valid_major(icd10("999")))
  expect_false(is_valid_major(as.icd10cm("999")))
})

test_that("major dispatch intact, white space not okay", {
  expect_true(is_valid_major(icd9("V10"), whitespace_ok = FALSE))
  expect_true(is_valid_major(as.icd9cm("V10"), whitespace_ok = FALSE))
  expect_true(is_valid_major(icd10("A00"), whitespace_ok = FALSE))
  expect_true(is_valid_major(as.icd10cm("A00"), whitespace_ok = FALSE))
  expect_false(is_valid_major(icd9(" V10"), whitespace_ok = FALSE))
  expect_false(is_valid_major(as.icd9cm("V10 "), whitespace_ok = FALSE))
  expect_false(is_valid_major(icd10(" A00"), whitespace_ok = FALSE))
  expect_false(is_valid_major(as.icd10cm("A00 "), whitespace_ok = FALSE))
})
