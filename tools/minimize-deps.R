# try to minimize dependency for some/all builds

# current minimal non-base R deps for ICD github:
# Rcpp RcppEigen

library(magrittr)
requireNamespace(c("igraph"), character.only = TRUE)
requireNamespace(c("miniCRAN"), character.only = TRUE)

# taken from DESCRIPTION initially
bse_deps <- c("methods", "utils", "stats", "graphics")
def_deps <- c("Rcpp", "RcppEigen")
# to build the vignettes (and therefore run the code therein)
vgn_deps <- c("knitr", "rmarkdown", "rticles", "magrittr")
# to build the documentation and run examples in a check, not roxygen2 stuff
# use of donttest and dontrun might be really helpful here
doc_deps <- c("magrittr")
tst_deps <- c("testthat")
# to download, generate data, generate documentation
dev_deps <- c("RODBC", "jsonlite", "xml2", "roxygen2", "readxl")
all_deps <- c(dev_deps, tst_deps, doc_deps, vgn_deps) # bse deps we can leave out here

basic_test_deps <- c(tst_deps, def_deps)
testthat_deps <- miniCRAN::makeDepGraph("testthat", suggests = FALSE) %>% miniCRAN:::plot.pkgDepGraph()
# assume we have these testthat deps, since we will definitely add testthat to the required packages list
miniCRAN::makeDepGraph(def_deps, suggests = FALSE) %>% miniCRAN:::plot.pkgDepGraph()
basic_test_pkgs <- miniCRAN::pkgDep(basic_test_deps, suggests = FALSE)

# okay, now to figure out a short list of rutter ppa cran packages that travis can install, esp to avoid macos compiling huge number of packages, like xml2, which often fail, and are barely needed.
#
# testthat deb already depends on: r-cran-cli, r-cran-crayon, r-cran-digest, r-cran-ellipsis, r-cran-evaluate, r-cran-magrittr, r-cran-pkgload, r-cran-praise, r-cran-r6, r-cran-rlang, r-cran-withr
