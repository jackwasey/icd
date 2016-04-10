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

context("deprecated filtering on POA")

test_that("deprecated - filter POA - not a data frame", {
  expect_error(icd9FilterPoaNo(list(pollo = "loco")))
  expect_error(icd9FilterPoaNotYes(visitId = c("1", "2"),
                                   code = c("1", "2"),
                                   presentonarrival = c("Y", "N")))
})

test_that("deprecated - filter POA - no poa field", {
  expect_error(icd9FilterPoaYes(simple_poa_pts[1:2]))
})

test_that("deprecated - filter POA - generic func - invalid poa type", {
  expect_error(icd9FilterPoa(x = simple_poa_pts,
                             poaField = "poa", poa = "not an option"))
  expect_error(icd9FilterPoa(x = simple_poa_pts,
                             poaField = "poa", poa = ""))
  expect_error(icd9FilterPoa(x = simple_poa_pts,
                             poaField = "poa", poa = NA))
})

test_that("deprecated - filter POA - wrong name poa field", {
  pd <- simple_poa_pts
  names(pd) <- c("visitId", "icd9", "achilleus")
  expect_error(icd9FilterPoaYes(pd, poaField = "poa"))
  expect_error(icd9FilterPoaYes(pd, poaField = "odysseus"))
  expect_error(icd9FilterPoaYes(pd))
})

test_that("deprecated - filter POA - poa is factor", {
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
  expect_identical(icd9FilterPoaYes(simple_poa_pts),
                   simple_poa_pts[1, 1:2])
  expect_identical(icd9FilterPoaNotYes(simple_poa_pts),
                   simple_poa_pts[-1, 1:2])
  expect_identical(icd9FilterPoaNo(simple_poa_pts),
                   simple_poa_pts[2, 1:2])
  expect_identical(icd9FilterPoaNotNo(simple_poa_pts),
                   simple_poa_pts[-2, 1:2])

  expect_identical(icd9FilterPoaYes(complex_poa_pts),
                   complex_poa_pts[1, 1:2])
  expect_identical(icd9FilterPoaNotYes(complex_poa_pts),
                   complex_poa_pts[-1, 1:2])
  expect_identical(icd9FilterPoaNo(complex_poa_pts),
                   complex_poa_pts[2, 1:2])
  expect_identical(icd9FilterPoaNotNo(complex_poa_pts),
                   complex_poa_pts[-2, 1:2])
})

test_that("deprecated - filter POA - poa is vector", {
  expect_identical(icd9FilterPoaYes(simple_poa_pts),
                   simple_poa_pts[1, 1:2])
  expect_identical(icd9FilterPoaNotYes(simple_poa_pts),
                   simple_poa_pts[-1, 1:2])
  expect_identical(icd9FilterPoaNo(simple_poa_pts),
                   simple_poa_pts[2, 1:2])
  expect_identical(icd9FilterPoaNotNo(simple_poa_pts),
                   simple_poa_pts[-2, 1:2])

  expect_identical(icd9FilterPoaYes(complex_poa_pts),
                   complex_poa_pts[1, 1:2])
  expect_identical(icd9FilterPoaNotYes(complex_poa_pts),
                   complex_poa_pts[-1, 1:2])
  expect_identical(icd9FilterPoaNo(complex_poa_pts),
                   complex_poa_pts[2, 1:2])
  expect_identical(icd9FilterPoaNotNo(complex_poa_pts),
                   complex_poa_pts[-2, 1:2])

  # same via core function
  expect_identical(icd9FilterPoa(complex_poa_pts, poa = "yes"),
                   complex_poa_pts[1, 1:2])
  expect_identical(icd9FilterPoa(complex_poa_pts, poa = "notYes"),
                   complex_poa_pts[-1, 1:2])
  expect_identical(icd9FilterPoa(complex_poa_pts, poa = "no"),
                   complex_poa_pts[2, 1:2])
  expect_identical(icd9FilterPoa(complex_poa_pts, poa = "notNo"),
                   complex_poa_pts[-2, 1:2])

})

test_that("deprecated - filter POA - poa upper and lower case", {
  smpl <- simple_poa_pts
  smpl[["poa"]] <- c("Y", "n", "e", NA)
  expect_identical(icd9FilterPoaNo(smpl), icd9FilterPoaNo(simple_poa_pts))
})

test_that("deprecated - filter POA - just Y and N should be complementary", {
  # take any data frame to start out:
  dfrm <- test_twenty;
  dfrm <- dfrm[dfrm[["poa"]] %in% c("Y", "N", "y", "n"), ]
  expect_identical(icd9FilterPoaNo(dfrm),  icd9FilterPoaNotYes(dfrm))
  expect_identical(icd9FilterPoaYes(dfrm), icd9FilterPoaNotNo(dfrm))
})
