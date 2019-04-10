# nocov start
.make_icd9cm_leaf_parsers()
.make_icd9cm_rtf_parsers()
.make_icd10cm_parsers()
.make_getters_and_fetchers()
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
  extra_msg <- if (system.file(package = "icd.data") != "") {
    paste("The ", sQuote("icd.data"), "")
  } else {
    ""
  }
  if (interactive() && .interact()) {
    packageStartupMessage(
      "icd downloads and caches data when needed. Use
setup_icd_data()
    to initialize the cache and enable automated downloads. Use:
download_icd_data()
    to cache everything at once, or complete an interrupted download. ",
      extra_msg
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
    "devtools::missing_s3()", # http://r-pkgs.had.co.nz/namespace.html
    "jwutil::jw_scan_build() or use .R/Makevars.clang.scan-build",
    # testing and compilation and different platforms:
    "Are there no skipped tests which should be run?",
    "Does it compile, test and check fine on travis and appveyor?",
    "Have you checked on Windows, win_builder (if possible with configure script), Mac, Ubuntu, rhub::check_with_sanitizers() etc", #nolint
    "Did you check with verbose, offline, interact all T/F",
    # final manual check:
    "Have all unnecessary files been ignored in built source package?"
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
