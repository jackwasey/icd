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

context("sysdata")

test_that("sysdata.rda is okay", {
  lknames <- c("icd9_short_n", "icd9_short_v", "icd9_short_e",
               "icd9_short_n_defined", "icd9_short_v_defined", "icd9_short_e_defined",
               "icd9_short_n_leaf", "icd9_short_v_leaf", "icd9_short_e_leaf",
               "icd9_sources", ".nc")

  sysdat <- generate_sysdata(save_data = FALSE)
  expect_equal(names(sysdat), lknames)

  expect_lt(length(icd9_short_n_leaf$vec), length(icd9_short_n_defined$vec))
  expect_lt(length(icd9_short_v_leaf$vec), length(icd9_short_v_defined$vec))
  expect_lt(length(icd9_short_e_leaf$vec), length(icd9_short_e_defined$vec))
  expect_lt(length(icd9_short_n_defined$vec), length(icd9_short_n$vec))
  expect_lt(length(icd9_short_v_defined$vec), length(icd9_short_v$vec))
  expect_lt(length(icd9_short_e_defined$vec), length(icd9_short_e$vec))
  expect_true(all(icd9_short_n_defined$env %eine% icd9_short_n$env))
  expect_true(all(icd9_short_v_defined$env %eine% icd9_short_v$env))
  expect_true(all(icd9_short_e_defined$env %eine% icd9_short_e$env))

  expect_equal(length(icd9_short_n$env), length(icd9_short_n$vec))
  expect_equal(length(icd9_short_v$env), length(icd9_short_v$vec))
  expect_equal(length(icd9_short_e$env), length(icd9_short_e$vec))
  expect_equal(length(icd9_short_n_defined$env), length(icd9_short_n_defined$vec))
  expect_equal(length(icd9_short_v_defined$env), length(icd9_short_v_defined$vec))
  expect_equal(length(icd9_short_e_defined$env), length(icd9_short_e_defined$vec))
  expect_equal(length(icd9_short_n_leaf$env), length(icd9_short_n_leaf$vec))
  expect_equal(length(icd9_short_v_leaf$env), length(icd9_short_v_leaf$vec))
  expect_equal(length(icd9_short_e_leaf$env), length(icd9_short_e_leaf$vec))
})
