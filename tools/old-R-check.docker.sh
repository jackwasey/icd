#!/bin/bash

# these steps to be run in docker container

apt update
apt install -y libcurl3-dev libxml2-dev
Rscript -e 'install.packages(c("docopt", "knitr", "checkmate", "testthat", "magrittr", "Rcpp"), repos="http://cran.rstudio.com")'
R CMD INSTALL --install-tests --no-test-load /mnt/icd_*.tar.gz
Rscript -e "library(icd); library(testthat); options('icd.do_slow_tests' = TRUE); test_check('icd')"
