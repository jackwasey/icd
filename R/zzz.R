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
    "Is there any commented code?",
    "Have commented tests been skipped or eliminated?",
    "Do all examples look ok (not just run without errors)?",
    "Anything to add to vignette?",
    "Have all the fixed github issues been closed",
    "Is NEWS.md updated?",
    "Is README.Rmd updated and recompiled into README.md?",
    "Have you compiled with clang and gcc with full warnings?",
    "Have you included updated copies of all offline versions of online data?",
    "Have you regenerated icd9Hierarchy and other compiled data, using parseAndSave?",
    "Are you happy with the code coverage?",
    "Have you linted?",
    "Does it compile and check fine on travis?",
    "Have you checked on Windows, win_builder, Mac, Ubuntu and UBSAN rocker?",
    "Are all NOTES from R CMD check documented in cran-comments.md",
    "Have all unnecessary files been ignored in built archive? Especially thinking of autoconfigure stuff."
  )
}

# EXCLUDE COVERAGE END
