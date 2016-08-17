pack <- available.packages()

# show all my recursive dependencies
tools::package_dependencies("icd9", which = c("Depends", "Imports", "LinkingTo"), db = pack, recursive = TRUE, reverse = FALSE)

# for my suggested packages, what are the hard dependencies of those packages?
cran_sug_pkgs <- tools::package_dependencies("icd9", which = c("Suggests"), db = pack, recursive = FALSE, reverse = FALSE)[[1]]
tools::package_dependencies(cran_sug_pkgs, which = c("Depends", "Imports", "LinkingTo"), db = pack, recursive = TRUE, reverse = FALSE) %>% unlist %>% unname %>% unique


# same but using the current install local 'icd' package, not the CRAN version:
imports <- packageDescription("icd", fields = "Imports") %>%
  strsplit(",|\n") %>%
  extract2(1) %>%
  grep(pattern = ".+", value = TRUE) %>%
  gsub(" .*$", "", .)
suggests <- packageDescription("icd", fields = "Suggests") %>%
  strsplit(",|\n") %>% extract2(1) %>%
  grep(pattern = ".+", value = TRUE) %>%
  gsub(" .*$", "", .)
tools::package_dependencies(imports,
                            which = c("Depends", "Imports", "LinkingTo"),
                            db = pack, recursive = TRUE, reverse = FALSE) %>%
  unlist %>% unname %>% unique %>% sort
tools::package_dependencies(suggests,
                            which = c("Depends", "Imports", "LinkingTo"),
                            db = pack, recursive = TRUE, reverse = FALSE) %>%
  unlist %>% unname %>% unique %>% sort
# lintr is very heavy on dependencies, used in development, but not suggested in
# DESCRIPTION.

# check dependencies of packages on which I depend or suggest
icd9dep <- pack["icd9", "Depends"] # based on current cran version, I assume
icd9imp <- pack["icd9", "Imports"]
icd9sug <- pack["icd9", "Suggests"]
icd9lnk <- pack["icd9", "LinkingTo"]

pack[c("Rcpp", "checkmate", "testthat", "knitr", "microbenchmark", "XML", "magrittr"), "Depends"]
pack[c("Rcpp", "checkmate", "testthat", "knitr", "microbenchmark", "XML", "magrittr"), "Imports"]

pack[c("Rcpp", "checkmate"), "Depends"]
pack[c("Rcpp", "checkmate"), "Imports"]
