# nocov start
.make_icd9cm_leaf_parsers()
.make_icd9cm_rtf_parsers()
.make_icd10cm_parsers()
.make_getters_and_fetchers()
# Set up an environment to cache chars_in_icd10cm
.lookup_chars_in_icd10cm <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  if (.icd_data_dir_okay() && is.null(.get_opt("offline"))) {
    .set_opt(offline = FALSE)
  }
  if (is.null(.get_opt("who_url"))) {
    .set_opt(who_url = "https://icd.who.int/browse10")
  }
}

.onAttach <- function(libname, pkgname) {
  if (system.file(package = "icd9") != "") {
    packageStartupMessage(
      paste(
        "The", sQuote("icd9"), "package is deprecated, and should be removed to",
        "avoid conflicts with ", sQuote("icd"), ". The", sQuote("icd"),
        "package up to version 2.1 contains",
        "tested versions of all the deprecated function names which overlap with",
        "those in the old", sQuote("icd9"), "package, e.g.,",
        sQuote("icd9ComorbidAhrq"), "'. It is",
        "highly recommended to run the command:",
        sQuote("remove.packages(\"icd9\")")
      )
    )
  }
  if (interactive()) {
    if (!.exists_icd_data_dir()) {
      packageStartupMessage(
        sQuote("icd"), " downloads data when needed. ",
        "set_icd_data_dir() creates a data directory. "
      )
      packageStartupMessage(
        "Default location is: ", sQuote(.default_icd_data_dir())
      )
    }
    if (system.file(package = "icd.data") != "") {
      packageStartupMessage(
        "N.b. the ", sQuote("icd.data"),
        " package is deprecated from ",
        sQuote("icd"), " version 4.0. ",
        "The content from ", sQuote("icd.data"),
        " is now available via ", sQuote("icd"), "."
      )
    }
  }
}

.onUnload <- function(libpath) {
  library.dynam.unload("icd", libpath)
}

release_questions <- function() {
  c(
    "run release_sanity_checks() in tools/extra-tests.R",
    # data
    "clean data then download and update everything on all platforms",
    # documentation:
    "manual render prebuilt efficiency & country-lang-vers vignettes,
    check correct in the tarball",
    "Do all examples look okay (not just run without errors)?",
    "Consider markdownlint mdl https://github.com/markdownlint/markdownlint",
    "tools/publish.sh and note check for broken links",
    # code quality:
    "tools/test-plus.sh",
    "Use clang scan-build, with latest version of clang (clang-tools in apt)",
    # testing and compilation and different platforms:
    "Are there any skipped tests which should be run?",
    "MacOS, Windows, Linux, r-hub, win-builder, travis, appveyor",
    "Download and set data dir in vanilla bash and R, Windows, Mac and Linux
    without library(icd) using icd::download_all_icd_data()",
    # final manual check:
    "Have all unnecessary files been ignored in built source tarball?",
    # post-release
    "Have all the fixed github issues been closed",
    "update version number to devel",
    "make new git branch for devel",
    "pkgdown::build_site() reports no errors"
  )
}

utils::globalVariables(c(
  "icd9_sub_chapters",
  "icd9_chapters",
  "icd9_majors",
  "icd10_sub_chapters",
  "icd10_chapters",
  "icd10cm2019",
  "icd9cm_hierarchy"
))

# nocov end
