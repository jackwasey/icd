pack <- available.packages()

# show all my recursive dependencies
tools::package_dependencies("icd9", which = c("Depends", "Imports", "LinkingTo"), db = pack, recursive = TRUE, reverse = FALSE)

# for my suggested packages, what are the hard dependencies of those packages?
sug_pkgs <- tools::package_dependencies("icd9", which = c("Suggests"), db = pack, recursive = FALSE, reverse = FALSE)[[1]]
tools::package_dependencies(sug_pkgs, which = c("Depends", "Imports", "LinkingTo"), db = pack, recursive = TRUE, reverse = FALSE)

# check dependencies of packages on which I depend or suggest

icd9dep <- pack["icd9", "Depends"] # based on current cran version, I assume
icd9imp <- pack["icd9", "Imports"]
icd9sug <- pack["icd9", "Suggests"]
icd9lnk <- pack["icd9", "LinkingTo"]

pack[c("Rcpp", "checkmate", "testthat", "knitr", "microbenchmark", "XML", "magrittr"), "Depends"]
pack[c("Rcpp", "checkmate", "testthat", "knitr", "microbenchmark", "XML", "magrittr"), "Imports"]

pack[c("Rcpp", "checkmate"), "Depends"]
pack[c("Rcpp", "checkmate"), "Imports"]
