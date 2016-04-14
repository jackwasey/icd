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

# nocov start

.onAttach <- function(libname, pkgname) {
  # nolint start
  packageStartupMessage(
    "Welcome to the \"icd\" package for finding comorbidities and interpretation of ICD-9 and ICD-10 codes. Suggestions and contributions are welcome at https://github.com/jackwasey/icd .

Available options are:
options(\"icd.warn_deprecated\" = TRUE) which will warn if deprecated functions from the old \"icd9\" package are used.
See the vignettes for examples.

Please cite this package if you find it useful in your published work.
citation(package = \"icd\")
")
  # nolint end

  if (system.file(package = "icd9") != "")
    packageStartupMessage(
      "The 'icd9' package is now deprecated, and should be removed to avoid conflicts with 'icd'.
The new 'icd' package contains tested versions of all the deprecated function names which overlap with those in the old
'icd9' package, e.g. 'icd9ComorbidAhrq'. It is strongly recommended to run the command:

            remove.packages(\"icd9\")", immediate. = TRUE, call. = FALSE)

  if (!is.null(getOption("icd9.threads")))
    packageStartupMessage("icd9.threads is deprecated, use icd.threads to control OpenMP")
  if (!is.null(getOption("icd9.chunkSize")))
    packageStartupMessage("icd9.chunkSize is deprecated, use icd.chunk_size to control OpenMP")
  if (!is.null(getOption("icd9.ompChunkSize")))
    packageStartupMessage("icd9.ompChunkSize is deprecated, use icd.omp_chunk_size to control OpenMP")

  if (!is.null(warn_deprecated <- getOption("icd.warn_deprecated"))) {
    if (warn_deprecated)
      packageStartupMessage("Deprecated function names from the old 'icd9' package will generate warnings.
                            Sometimes many warnings are generated for one call. To turn this off, use:
                            option(icd.warn_deprecated = FALSE)")
    else
      packageStartupMessage("Deprecated function names from the old 'icd9' package will not generate warnings.
                            This can result in conflicts with the old package (if still installed), and these
                            functions will be removed. The new function names were needed for consistency when
                            adding icd10 support, and conform to the popular coding style used by Hadley Wickham:
                            http://adv-r.had.co.nz/Style.html . To warn for deprecated functions, use:
                            option(icd.warn_deprecated = TRUE)")

  }
}

.onUnload <- function(libpath) {
  library.dynam.unload("icd", libpath)
}

release_questions <- function() {
  c(
    # data:
    "Have you regenerated icd9cm_hierarchy and other compiled data on Linux?",
    "Uranium data requires rebuild on Windows for RODBC to extract raw data",
    "Have you run tools::checkRdaFiles(\"data\") to check everything is optimally compressed?",
    # documentation:
    "Have you checked all TODO comments, made into github issues where appropriate",
    "Do all examples look ok (not just run without errors)?",
    "Vignette has updated description paragraph, in sync with DESCRIPTION",
    "Have all the fixed github issues been closed",
    "Is NEWS.md updated and contributors credited?",
    "Is README.Rmd updated and recompiled into README.md?",
    "Does every file have correct licence information?",
    "Are github pages site refreshed from latest documentation?",
    "Is spelling correct everywhere? E.g. aspell_package_Rd_files('.')",
    # code quality:
    "Has the development branch been merged/rebased into master?",
    "Are you happy with the code coverage?",
    "Is every SEXP PROTECT()ed and UNPROTECT()ed, when appropriate?",
    "Is the naming scheme consistent, with deprecated old style names?",
    "Are all public S3 classes all exported? use devtools::missing_s3()",
    "use LLVM scan build by adding 'scan-build before compiler path in .R/Makevars",
    # testing and compilation and different platforms:
    "Have you run with testthat <= and > 0.11.0?",
    "Have you run autoreconf before building and testing?",
    "Has config.h.win been updated to reflect latest configure.ac results?",
    "Are there skipped tests which should be run?",
    "Have tests been run with slow and online tests turned on?",
    "Does it compile and check fine on travis and wercker?",
    "Is build_install_check_in_docker pointing at the correct branch?",
    "Have you checked on Windows, win_builder (if possible with configure step),
      Mac, Ubuntu, UBSAN rocker, and updated my docker image which
      resembles a CRAN maintainers environment?",
    "Have you compiled with clang and gcc with full warnings and pedantic
      (normally done by UBSAN builds anyway)?",
    # final manual check:
    "Are all NOTES from R CMD check documented in cran-comments.md",
    "Have all unnecessary files been ignored in built archive? Especially
      thinking of autoconfigure stuff. Look in the final built archive
      before submitting to CRAN?",
    NULL
  )
}

# nocov end
