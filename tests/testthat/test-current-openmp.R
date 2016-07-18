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

context("OpenMP thread and chunk parameters")
# OpenMP already used, if compiled in, bu t need some specific stress tests, e.g for error in bug #75

test_that("single icd9 code comorbidity", {
  # this is enough to segfault with clang 3.7, libc++ and libomp
  x <- data.frame(visit_id = "a", icd9 = "441")
  icd9_comorbid_quan_deyo(x, short_code = FALSE, applyHierarchy = T)
})

test_that("thousands of patients", {
  x <- generate_random_pts(10000)
  icd9_comorbid_quan_deyo(x, short_code = FALSE, applyHierarchy = TRUE)
})

test_that("vary everything", {
  old_opts <- options("icd.threads", "icd.chunk_size", "icd.omp_chunk_size")
  omp_chunk_size <- 1
  # prime numbers
  for (pts in c(0, 1, 3, 31, 1013, 10009)) {
    for (dz_per_patient in c(1, 23)) {
      rand_pts <- generate_random_unordered_pts(pts, dz_per_patient)
      for (threads in c(1, 5, 9)) {
        for (chunk_size in c(1, 2, 11, 29, 101, 997, 10007)) {
          options("icd.threads" = threads)
          options("icd.chunk_size" = chunk_size)
          # omp_chunk_size is not currently not set in CPP code
          options("icd.omp_chunk_size" = omp_chunk_size)
          expect_error(icd9_comorbid_quan_deyo(rand_pts, short_code = FALSE, hierarchy = TRUE),
                       regexp = NA,
                       info = sprintf("pts = %i, dz_per_patient = %i, threads = %i, chunk_size = %i",
                                      pts, dz_per_patient, threads, chunk_size))
        }
      }
    }
  }
  options(old_opts)
})
