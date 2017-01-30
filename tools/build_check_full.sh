#!/bin/bash
pushd /tmp
Rscript -e 'devtools::install_github("jackwasey/jwutil")'
Rscript -e 'devtools::install_github("jackwasey/icdData")'
R CMD build ~/icd
ICD_SLOW_TESTS=TRUE R CMD check --as-cran "$(ls -t /tmp/icd*.tar.gz | head -1)"
popd
