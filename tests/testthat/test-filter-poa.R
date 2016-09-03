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

context("filtering on POA")

test_that("filter POA - not a data frame", {
  expect_error(icd_filter_poa_no(list(pollo = "loco")))
})

test_that("filter POA - no poa field", {
  expect_error(icd_filter_poa_yes(simple_poa_pts[1:2]))
})

test_that("filter POA - generic func - invalid poa type", {
  expect_error(icd_filter_poa(x = simple_poa_pts,
                             poaField = "poa", poa = "not an option"))
  expect_error(icd_filter_poa(x = simple_poa_pts,
                             poaField = "poa", poa = ""))
  expect_error(icd_filter_poa(x = simple_poa_pts,
                             poaField = "poa", poa = NA))
})

test_that("filter POA - wrong name poa field", {
  pd <- simple_poa_pts
  names(pd) <- c("visit_id", "icd9", "achilleus")
  expect_error(icd_filter_poa_yes(pd, poaField = "poa"))
  expect_error(icd_filter_poa_yes(pd, poaField = "odysseus"))
  expect_error(icd_filter_poa_yes(pd))
})

test_that("filter POA - poa is factor", {
  # POA flag is an obvious case for using factors. Not sure if it saves much
  # memory, and it certainly risks screwing up the analysis with obscure and
  # difficult to debug errors. ICD-9 code is also factor fodder, and likely to
  # be highly repeated over millions of patients, but I've resisted its charms
  # thus far.

  # just within this closure
  simple_poa_pts$poa <- factor(simple_poa_pts$poa)
  names(simple_poa_pts)[3] <- "poa"
  # just within this closure
  complex_poa_pts$poa <- factor(complex_poa_pts$poa)
  names(complex_poa_pts)[3] <- "poa"

  # row names are preserved here: probably not important, but a little annoying
  expect_identical(icd_filter_poa_yes(simple_poa_pts),
                   simple_poa_pts[1, 1:2])
  expect_identical(icd_filter_poa_not_yes(simple_poa_pts),
                   simple_poa_pts[-1, 1:2])
  expect_identical(icd_filter_poa_no(simple_poa_pts),
                   simple_poa_pts[2, 1:2])
  expect_identical(icd_filter_poa_not_no(simple_poa_pts),
                   simple_poa_pts[-2, 1:2])

  expect_identical(icd_filter_poa_yes(complex_poa_pts),
                   complex_poa_pts[1, 1:2])
  expect_identical(icd_filter_poa_not_yes(complex_poa_pts),
                   complex_poa_pts[-1, 1:2])
  expect_identical(icd_filter_poa_no(complex_poa_pts),
                   complex_poa_pts[2, 1:2])
  expect_identical(icd_filter_poa_not_no(complex_poa_pts),
                   complex_poa_pts[-2, 1:2])
})

test_that("filter POA - poa is vector", {
  expect_identical(icd_filter_poa_yes(simple_poa_pts),
                   simple_poa_pts[1, 1:2])
  expect_identical(icd_filter_poa_not_yes(simple_poa_pts),
                   simple_poa_pts[-1, 1:2])
  expect_identical(icd_filter_poa_no(simple_poa_pts),
                   simple_poa_pts[2, 1:2])
  expect_identical(icd_filter_poa_not_no(simple_poa_pts),
                   simple_poa_pts[-2, 1:2])

  expect_identical(icd_filter_poa_yes(complex_poa_pts),
                   complex_poa_pts[1, 1:2])
  expect_identical(icd_filter_poa_not_yes(complex_poa_pts),
                   complex_poa_pts[-1, 1:2])
  expect_identical(icd_filter_poa_no(complex_poa_pts),
                   complex_poa_pts[2, 1:2])
  expect_identical(icd_filter_poa_not_no(complex_poa_pts),
                   complex_poa_pts[-2, 1:2])

  # same via core function
  expect_identical(icd_filter_poa(complex_poa_pts, poa = "yes"),
                   complex_poa_pts[1, 1:2])
  expect_identical(icd_filter_poa(complex_poa_pts, poa = "notYes"),
                   complex_poa_pts[-1, 1:2])
  expect_identical(icd_filter_poa(complex_poa_pts, poa = "no"),
                   complex_poa_pts[2, 1:2])
  expect_identical(icd_filter_poa(complex_poa_pts, poa = "notNo"),
                   complex_poa_pts[-2, 1:2])

})

test_that("filter POA - poa upper and lower case", {
  smpl <- simple_poa_pts
  smpl[["poa"]] <- c("Y", "n", "e", NA)
  expect_identical(icd_filter_poa_no(smpl), icd_filter_poa_no(simple_poa_pts))
})

test_that("filter POA - just Y and N should be complementary", {
  # take any data frame to start out:
  dfrm <- test_twenty;
  dfrm <- dfrm[dfrm[["poa"]] %in% c("Y", "N", "y", "n"), ]
  expect_identical(icd_filter_poa_no(dfrm),  icd_filter_poa_not_yes(dfrm))
  expect_identical(icd_filter_poa_yes(dfrm), icd_filter_poa_not_no(dfrm))
})
