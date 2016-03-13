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

context("C++")

test_that("C++ tests all pass", {
  # this does require a recent version of testthat > 0.11.0.9000
  catch_avail <- tryCatch(
    is.list(getNativeSymbolInfo("run_testthat_tests", PACKAGE = "icd")),
    error = function(e) FALSE
  )
  if (!catch_avail)
    skip("recent testthat with C++ Catch not available")

  # further check that we have this testthat function available
  if (length(find("expect_cpp_tests_pass", mode = "function")) > 0)
    testthat::expect_cpp_tests_pass("icd")
})
