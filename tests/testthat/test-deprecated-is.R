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

context("deprecated sub-types of ICD-9 code")

vs <- c("V1", "V99", " V05", "v19x", " v200 ")
es <- c("E00", "E9999", " E905", "e901", " e888 ")
ns <- c("0.1", "1.2", " 2", "33", " 444XX", "555.5", "66666 ", "789.01")

test_that("deprecated - find codes", {
  expect_true(icd9IsV(" V10 "))
  expect_true(icd9IsE(" E800"))
  expect_true(icd9IsN(" 10.1"))
  expect_true(icd9IsMajor(" 100"))
  expect_true(icd9IsMajor(" E900"))
  expect_false(icd9IsMajor(" V90.3 "))
  expect_false(icd9IsMajor(" E900.3 "))
  # todo, what about "E800." or "100." ?
  expect_false(icd9IsV(" E900.3 "))
  expect_false(icd9IsE(" 80.2"))
  expect_false(icd9IsN(" V10.1"))
})

test_that("deprecated - 'is' works for multiple values", {
  expect_true(all(icd9IsV(vs)))
  expect_true(all(icd9IsE(es)))
  expect_true(all(icd9IsN(ns)))
  expect_false(all(icd9IsV(ns)))
  expect_false(all(icd9IsE(vs)))
  expect_false(all(icd9IsN(es)))

  expect_false(any(icd9IsN(c(vs, es))))
})

test_that("deprecated - 'is' works for factors", {
  expect_true(all(icd9IsV(factor(vs))))
  expect_true(all(icd9IsE(factor(es))))
  expect_true(all(icd9IsN(factor(ns))))
  expect_false(all(icd9IsV(factor(ns))))
  expect_false(all(icd9IsE(factor(vs))))
  expect_false(all(icd9IsN(factor(es))))
})

test_that("deprecated - 'is' mixed values, factors and vectors", {
  v <- c("E100.1", "V234", "12", "V34.2", "61523", "10.2", "E9991", " V45XX ")
  expect_equal(icd9IsN(v), c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(icd9IsV(v), c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE))
  expect_equal(icd9IsE(v), c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))
})
