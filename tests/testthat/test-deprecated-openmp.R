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

context("OpenMP tests")
# OpenMP already used, if compiled in, bu t need some specific stress tests, e.g for error in bug #75

test_that("single icd9 code comorbidity", {
  # this is enough to segfault with clang 3.7, libc++ and libomp
  x <- data.frame(visitId = "a", icd9 = "441")
  icd9ComorbidQuanDeyo(x, isShort = F, applyHierarchy = T)
})

test_that("thousands of patients", {
  x <- randomPatients(10000)
  expect_that(
    icd9ComorbidQuanDeyo(x, isShort = F, applyHierarchy = T),
    testthat::not(testthat::throws_error())
  )
})

test_that("vary everything", {
  ompChunkSize = 1
  # prime numbers
  for (pts in c(0, 1, 3, 31, 1013, 10009)) {
    for (dz_per_patient in c(1, 23)) {
      for (threads in c(1, 5, 9)) {
        for (chunkSize in c(1, 2, 11, 29, 101, 997, 10007)) {
          options("icd9.threads" = threads)
          options("icd9.chunkSize" = chunkSize)
          # ompChunkSize is not currently not set in CPP code
          options("icd9.ompChunkSize" = ompChunkSize)
          expect_that(
            icd9ComorbidQuanDeyo(randomUnorderedPatients(pts, dz_per_patient),
                                 isShort = FALSE, applyHierarchy = TRUE),
            testthat::not(throws_error()),
            info = sprintf("pts = %i, dz_per_patient = %i, threads = %i, chunkSize = %i",
                           pts, dz_per_patient, threads, chunkSize)
          )
        }
      }
    }
  }
})
