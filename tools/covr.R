#!/usr/bin/env Rscript

# simple script to encapsulate coverage code for different build systems
# Enables build matrix not to do code coverage for each permutation.

# first argument is "true" or "false"
# second argument is "codecov" or "coveralls"

args = commandArgs(trailingOnly = TRUE)
if (tolower(args[[1]]) == "true") {

  options(warn = -1)

  if (args[[2]] == "coveralls")
    covr::coveralls(quiet = FALSE)
  else if (args[[2]] == "codecov")
    covr::codecov(quiet = FALSE)
  else
    stop("Must use codecov or coveralls.")

  covr::package_coverage(type = "vignette", quiet = FALSE)
} else {
  message("Not running code coverage")
}
cat(0)
