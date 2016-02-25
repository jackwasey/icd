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

library("icd")
library("testthat", warn.conflicts = FALSE, quietly = TRUE)
library("magrittr", warn.conflicts = FALSE, quietly = TRUE)

if (FALSE) {
  message("lib paths are as follows:")
  print(.libPaths())
}

# to run all tests:
if (FALSE) {
  print(options("icd.do_slow_tests"))
  print(options("icd.do_online_tests"))
  print(options("icd.warn_deprecated"))
  options("icd.do_slow_tests" = TRUE)
  options("icd.do_online_tests" = TRUE)
  options("icd.warn_deprecated" = FALSE)
}

if (is.null(options("icd.do_slow_tests")))
  options("icd.do_slow_tests" = FALSE)
if (is.null(options("icd.do_online_tests")))
  options("icd.do_online_tests" = FALSE)
if (is.null(options("icd.warn_deprecated")))
  options("icd.warn_deprecated" = TRUE)

if (identical(Sys.getenv("COVR"), "true")) {
  message("my environment variable COVR found so doing slow and online tests")
  options("icd.do_slow_tests" = TRUE)
  options("icd.do_online_tests" = TRUE)
  options("icd.warn_deprecated" = TRUE)
}

if (identical(tolower(Sys.getenv("ICD_SLOW_TESTS")), "true")) {
  message("environment variable ICD_SLOW_TESTS found to be true, so doing slow tests")
  options("icd.do_slow_tests" = TRUE)
}

if (identical(tolower(Sys.getenv("ICD_ONLINE_TESTS")), "true")) {
  message("environment variable ICD_ONLINE_TESTS found to be true, so doing online tests")
  options("icd.do_online_tests" = TRUE)
}

if (identical(tolower(Sys.getenv("ICD_WARN_DEPRECATED")), "true")) {
  message("environment variable ICD_WARN_DEPRECATE found to be true, so warning for deprecated icd9 function use")
  options("icd.warn_deprecated" = TRUE)
}

# try to disable warning, as many deprecated tests do warn. This appears not to
# work when test that runs the tests.
old_warn <- options(warn = -1)
on.exit(options(old_warn))

# use summary reporter so that covr produces output and doesn't time-out on
# travis. The code coverage testing is slower than regular testing because of
# instrumentation.

# standard reporter to use is Summary
test_check("icd", filter = "icd",
           reporter = testthat::MultiReporter(reporters = list(testthat::SummaryReporter(),
                                                               testthat::StopReporter()))
)
