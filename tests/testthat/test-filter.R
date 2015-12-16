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

context("test filtering on POA")

test_that("filter POA - not a data frame", {
  expect_error(icd9FilterPoaNo(list(pollo = "loco")))
  expect_error(icd9FilterPoaNotYes(visitId=c("1","2"),
                                   icd9 = c("1","2"),
                                   poa = c("Y","N")))
})

test_that("filter POA - no poa field", {
  expect_error(icd9FilterPoaYes(simplePoaPatients[1:2]))
})

test_that("filter POA - generic func - invalid poa type", {
  expect_error(icd9FilterPoa(x = simplePoaPatients,
                             poaField = "poa", poa = "not an option"))
  expect_error(icd9FilterPoa(x = simplePoaPatients,
                             poaField = "poa", poa = ""))
  expect_error(icd9FilterPoa(x = simplePoaPatients,
                             poaField = "poa", poa = NA))
})

test_that("filter POA - wrong name poa field", {
  pd <- simplePoaPatients
  names(pd) <- c("visitId", "icd9", "achilleus")
  expect_error(icd9FilterPoaYes(pd, poaField = "poa"))
  expect_error(icd9FilterPoaYes(pd, poaField = "odysseus"))
  expect_error(icd9FilterPoaYes(pd))
})

test_that("filter POA - poa is factor", {
  # POA flag is an obvious case for using factors. Not sure if it saves much
  # memory, and it certainly risks screwing up the analysis with obscure and
  # difficult to debug errors. ICD-9 code is also factor fodder, and likely to
  # be highly repeated over millions of patients, but I've resisted its charms
  # thus far.

  # just within this closure
  simplePoaPatients$poa <- factor(simplePoaPatients$poa)
  names(simplePoaPatients)[3] <- "poa"
  # just within this closure
  complexPoaPatients$poa <- factor(complexPoaPatients$poa)
  names(complexPoaPatients)[3] <- "poa"

  # row names are preserved here: probably not important, but a little annoying
  expect_identical(icd9FilterPoaYes(simplePoaPatients),
                   simplePoaPatients[1, 1:2])
  expect_identical(icd9FilterPoaNotYes(simplePoaPatients),
                   simplePoaPatients[-1, 1:2])
  expect_identical(icd9FilterPoaNo(simplePoaPatients),
                   simplePoaPatients[2, 1:2])
  expect_identical(icd9FilterPoaNotNo(simplePoaPatients),
                   simplePoaPatients[-2, 1:2])

  expect_identical(icd9FilterPoaYes(complexPoaPatients),
                   complexPoaPatients[1, 1:2])
  expect_identical(icd9FilterPoaNotYes(complexPoaPatients),
                   complexPoaPatients[-1, 1:2])
  expect_identical(icd9FilterPoaNo(complexPoaPatients),
                   complexPoaPatients[2, 1:2])
  expect_identical(icd9FilterPoaNotNo(complexPoaPatients),
                   complexPoaPatients[-2, 1:2])
})

test_that("filter POA - poa is vector", {
  expect_identical(icd9FilterPoaYes(simplePoaPatients),
                   simplePoaPatients[1, 1:2])
  expect_identical(icd9FilterPoaNotYes(simplePoaPatients),
                   simplePoaPatients[-1, 1:2])
  expect_identical(icd9FilterPoaNo(simplePoaPatients),
                   simplePoaPatients[2, 1:2])
  expect_identical(icd9FilterPoaNotNo(simplePoaPatients),
                   simplePoaPatients[-2, 1:2])

  expect_identical(icd9FilterPoaYes(complexPoaPatients),
                   complexPoaPatients[1, 1:2])
  expect_identical(icd9FilterPoaNotYes(complexPoaPatients),
                   complexPoaPatients[-1, 1:2])
  expect_identical(icd9FilterPoaNo(complexPoaPatients),
                   complexPoaPatients[2, 1:2])
  expect_identical(icd9FilterPoaNotNo(complexPoaPatients),
                   complexPoaPatients[-2, 1:2])

  # same via core function
  expect_identical(icd9FilterPoa(complexPoaPatients, poa = "yes"),
                   complexPoaPatients[1, 1:2])
  expect_identical(icd9FilterPoa(complexPoaPatients, poa = "notYes"),
                   complexPoaPatients[-1, 1:2])
  expect_identical(icd9FilterPoa(complexPoaPatients, poa = "no"),
                   complexPoaPatients[2, 1:2])
  expect_identical(icd9FilterPoa(complexPoaPatients, poa = "notNo"),
                   complexPoaPatients[-2, 1:2])

})

test_that("filter POA - poa upper and lower case", {
  smpl <- simplePoaPatients
  smpl[["poa"]] <- c("Y", "n", "e", NA)
  expect_identical(icd9FilterPoaNo(smpl), icd9FilterPoaNo(simplePoaPatients))
})

test_that("filter POA - just Y and N should be complementary", {
  # take any data frame to start out:
  dfrm <- testTwenty;
  dfrm <- dfrm[dfrm[["poa"]] %in% c("Y", "N", "y", "n"),]
  expect_identical(icd9FilterPoaNo(dfrm),  icd9FilterPoaNotYes(dfrm))
  expect_identical(icd9FilterPoaYes(dfrm), icd9FilterPoaNotNo(dfrm))
})
