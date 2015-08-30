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

# EXCLUDE COVERAGE START

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Welcome to the icd9 package for finding comorbidities and interpretation of ICD-9 codes.
Suggestions and contributions are welcome at https://github.com/jackwasey/icd9 .

Please cite this package if you find it useful for your published work.
citation(package = \"icd9\")

Development version can be installed with
# install.packages(\"devtools\")
library(devtools)
install_github(\"jackwasey/icd9\")
")
}

.onUnload <- function(libpath) {
  library.dynam.unload("icd9", libpath)
}

release_questions <- function() {
  c(
    "Have you checked all TODO comments",
    "Is there any commented code? (lint for this now possible)",
    "Have tests been run with do_slow_tests turned on?",
    "Are there skipped tests which should be run?",
    "Do all examples look ok (not just run without errors)?",
    "Anything to add to vignette?",
    "Have all the fixed github issues been closed",
    "Is NEWS.md updated?",
    "Is README.Rmd updated and recompiled into README.md?",
    "Have you compiled with clang and gcc with full warnings?",
    "Have you included updated copies of all offline versions of online data?",
    "Have you regenerated icd9Hierarchy and other compiled data?",
    "Are you happy with the code coverage?",
    "Does every file have correct licence information?",
    "Have you linted?",
    "Does it compile and check fine on travis?",
    "Have you checked on Windows, win_builder (if possible with configure step), Mac, Ubuntu, ASAN, and UBSAN rocker, and updated my docker image which resembles a CRAN maintainers environment?",
    "Are all NOTES from R CMD check documented in cran-comments.md",
    "Have all unnecessary files been ignored in built archive? Especially thinking of autoconfigure stuff."
  )
}

# EXCLUDE COVERAGE END
