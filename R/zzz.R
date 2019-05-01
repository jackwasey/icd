# nocov start
.make_icd9cm_leaf_parsers()
.make_icd9cm_rtf_parsers()
.make_icd10cm_parsers()
.make_getters_and_fetchers()
# Set up an environment to cache chars_in_icd10cm
.lookup_chars_in_icd10cm <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  if (.icd_data_dir_okay()) {
    .set_opt(offline = FALSE, overwrite = FALSE)
  }
  if (is.null(getOption("icd.data.who_url"))) {
    options("icd.data.who_url" = "https://icd.who.int/browse10")
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
    # vignette
    "manual rebuild efficiency & country-lang-vers vignettes, check in tarball",
    # commands:
    ".clean(destroy = TRUE), then update_everything() on all platforms",
    "aspell_package_Rd_files('.')",
    # documentation:
    "Check all TODO comments, make into github issues",
    "Do all examples look ok (not just run without errors)?",
    "Have all the fixed github issues been closed",
    "pkgdown::build_site() reports no errors",
    # code quality:
    "icd::download_all_icd_data() and other major functions pre-library call",
    "codetools::checkUsagePackage('icd', all = TRUE, suppressLocal = TRUE)",
    "devtools::missing_s3()", # http://r-pkgs.had.co.nz/namespace.html
    "Use clang scan-build, with latest version of clang",
    # testing and compilation and different platforms:
    "Are there no skipped tests which should be run?",
    "rhub::check_with_sanitizers()",
    "rhub::check_for_cran()",
    "Windows? Appveyor, win_builder, r-hub all okay?",
    "MacOS? Travis and local okay?",
    "Linux? Travis, r-hub okay?",
    "Check with verbose, offline, interact all undefined in vanilla R",
    "styler::style_pkg()",
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
