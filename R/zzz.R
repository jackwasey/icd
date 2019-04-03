# nocov start
.verbose(getOption("icd.data.verbose", default = FALSE))
.make_icd9cm_leaf_parsers()
.make_icd9cm_rtf_parsers()
.make_icd10cm_parsers()
# get_ and .get_ functions only depend  on the data name
.make_getters_and_fetchers()

.onLoad <- function(libname, pkgname) {
  .set_init_options()
}

# Set up an environment to cache chars_in_icd10cm
.lookup_chars_in_icd10cm <- new.env(parent = emptyenv())

.onAttach <- function(libname, pkgname) {
  if (system.file(package = "icd9") != "") {
    packageStartupMessage(paste(
      "The 'icd9' package is now deprecated, and should be removed to avoid",
      "conflicts with 'icd'. The 'icd' package up to version 2.1 contains",
      "tested versions of all the deprecated function names which overlap with",
      "those in the old 'icd9' package, e.g. 'icd9ComorbidAhrq'. It is",
      "strongly recommended to run the command: remove.packages(\"icd9\")"
    ))
  }
  if (interactive() && .interact()) {
    packageStartupMessage(
      "icd downloads and caches data when needed. Use
setup_icd_data()
    to initialize the cache and enable automated downloads. Use:
download_icd_data()
    to cache everything at once, or complete an interrupted download."
    )
  }
  if (.interact() && !.all_cached()) {
    packageStartupMessage(
      "Not all the available ICD-9-CM data has been downloaded. To complete the download and parsing process use:
download_icd_data()"
    )
  }
}

.onUnload <- function(libpath) {
  library.dynam.unload("icd", libpath)
}

release_questions <- function() {
  c( # vignette
    "manual rebuild of efficiency and country-lang-vers vignettes",
    # commands:
    "update_everything() on linux",
    "aspell_package_Rd_files('.')",
    # documentation:
    "Check all TODO comments, make into github issues",
    "Do all examples look ok (not just run without errors)?",
    "Have all the fixed github issues been closed",
    # code quality:
    "codetools::checkUsagePackage('icd', all = TRUE, suppressLocal = TRUE)",
    "styler::style_pkg()",
    "Are all public S3 classes all exported? use devtools::missing_s3() http://r-pkgs.had.co.nz/namespace.html",
    "use LLVM static scan build, scan-build before compiler in .R/Makevars",
    # testing and compilation and different platforms:
    "local install, all tests pass with data all downloaded and parsed",
    "Have you run tests in tests-deprecated and tests-build-code?",
    "Are there no skipped tests which should be run?",
    "Does it compile, test and check fine on travis and appveyor?",
    "Have you checked on Windows, win_builder,
      Mac, Ubuntu, rhub::check_with_sanitizers() etc",
    "Consider whether to include Catch headers in submitted package",
    # final manual check:
    "Are all NOTES from R CMD check documented in cran-comments.md",
    "Have all unnecessary files been ignored in built archive?"
  )
}

utils::globalVariables(c(
  "icd9_sub_chapters",
  "icd9_chapters",
  "icd9_majors",
  "icd10_sub_chapters",
  "icd10_chapters",
  "icd10cm2016",
  "icd10cm2019",
  "icd9cm_hierarchy"
))

# nocov end
