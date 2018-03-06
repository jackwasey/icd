#!/usr/bin/env Rscript

# simple script to encapsulate coverage code for different build systems
# Enables build matrix not to do code coverage for each permutation.

args = commandArgs(trailingOnly = TRUE)
message("Running tools/covr.R with args: ", paste(args, collapse = ", "))

if (length(args) != 2)
  stop("first argument is 'true' or 'false' string, indicating whether to run coverage
       second argument is package_coverage (for shiny output), coveralls or codecov.")

if (tolower(args[[1]]) == "true") {

  # do give warnings as they arise
  options(warn = 1)

  # icd not loaded so can't just set the option via icd::do_slow_tests. Try:
  Sys.setenv(ICD_SLOW_TESTS = "false")

  if (args[[2]] == "coveralls")
    covr::coveralls(quiet = FALSE)
  else if (args[[2]] == "codecov")
    covr::codecov(quiet = FALSE)
  else if (args[[2]] == "package_coverage")
    covr::report(covr::package_coverage())
  else
    stop("Must use package_coverage, codecov or coveralls.")

  covr::package_coverage(type = "vignette", quiet = FALSE)
} else {
  message("Not running code coverage")
}
cat(0)
