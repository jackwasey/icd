# check dependencies of packages on which I depend or suggest
pack <- available.packages()

icd9dep <- pack["icd9", "Depends"] # based on current cran version, I assume
icd9imp <- pack["icd9", "Imports"]
icd9sug <- pack["icd9", "Suggests"]
icd9lnk <- pack["icd9", "LinkingTo"]

pack[c("Rcpp", "checkmate", "testthat", "knitr", "microbenchmark", "XML", "magrittr", "memoise"), "Depends"]
pack[c("Rcpp", "checkmate", "testthat", "knitr", "microbenchmark", "XML", "magrittr", "memoise"), "Imports"]

pack[c("Rcpp", "checkmate"), "Depends"]
pack[c("Rcpp", "checkmate"), "Imports"]
