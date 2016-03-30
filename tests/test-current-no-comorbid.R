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

# this is common code to all the tests, each of which runs test_check with a different filter:

library("icd")
library("testthat", warn.conflicts = FALSE, quietly = TRUE)
library("magrittr", warn.conflicts = FALSE, quietly = TRUE)

# we now rely on a testthat version with backwards-incompatible changes, the whole of the expectation setup has changed,
# and there are many deprecations. For now, only run tests if testthat version is high enough:
#
if (packageVersion("testthat") < package_version("0.11.0.9000")) {
  message("testthat version is less than 0.11.0.9000, so not running Catch tests. Consider using:
          devtools::install_github('hadley/testthat')")
}

# when covr runs tests, it installs the package with source, and sources the files in the test directory.
# This means that /data-raw would be absent, so it should be in inst/data-raw for testing.

icd:::setup_test_check()
icd:::show_test_options()
# http://stackoverflow.com/questions/406230/regular-expression-to-match-line-that-doesnt-contain-a-word
icd:::my_test_check("current-((?!comorbid).)*$", "Running current tests without comorbid")
