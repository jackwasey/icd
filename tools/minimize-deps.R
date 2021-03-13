# try to minimize dependency for some/all builds

# current minimal non-base R deps for ICD github:
# Rcpp RcppEigen

library(magrittr)
requireNamespace(c("igraph"), character.only = TRUE)
requireNamespace(c("pkgdepends"), character.only = TRUE)

#cran_icd_gr <- miniCRAN::makeDepGraph("icd", suggests = TRUE)
#miniCRAN:::plot.pkgDepGraph(cran_icd_dgr)

# taken from DESCRIPTION initially
bse_deps <- c("methods", "utils", "stats", "graphics")
def_deps <- c("Rcpp", "RcppEigen")
# to build the vignettes (and therefore run the code therein)
vgn_deps_cran <- c("knitr", "rmarkdown", "rticles", "magrittr")
vgn_deps_offline <- "rticles"
vgn_deps_all <- c(vgn_deps_cran, vgn_deps_offline)
# to build the documentation and run examples in a check, not roxygen2 stuff
# use of donttest and dontrun might be really helpful here
doc_deps <- c("magrittr", "roxygen2")
tst_deps <- c("testthat")
# to download, generate data, generate documentation
off_dat_deps <- c("RODBC, xml2")
run_dat_deps <- c("jsonlite", "readxl")
# bse deps we can leave out here
all_deps <- c(def_deps,
              tst_deps,
              vgn_deps_all,
              doc_deps,
              off_dat_deps,
              run_dat_deps,
              off_dat_deps
              )

cran_sees_deps <- unique(c(def_deps, vgn_deps_cran, doc_deps, run_dat_deps))
cran_uses_deps <- unique(c(def_deps, vgn_deps_cran, doc_deps))

basic_test_deps <- c(tst_deps, def_deps)
deps <- pkgdepends::new_pkg_deps(basic_test_deps)
deps$solve()
deps$draw()

cdep <- pkgdepends::new_pkg_deps(cran_sees_deps)
cdep$solve()
cdep$draw()
