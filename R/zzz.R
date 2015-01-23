# EXCLUDE COVERAGE
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
"Welcome to the icd9 package for finding comorbidities and interpretation of ICD-9 codes.
Suggestions and contributions are welcome at https://github.com/jackwasey/icd9 .

Please cite this package if you find it useful for your published work.
citation(package = \"icd9\")

Development version can be installed with
# install.packages(\"devtools\")
library(devtools)
install_github(\"jackwasey/icd9\")
")
}
