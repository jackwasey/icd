#!/bin/sh
cd /tmp
git clone https://github.com/jackwasey/icd9.git
#Rscript -e 'install.packages(c("Rcpp", "checkmate", "stats", "utils"))'
#Rscript -e 'install.packages(c("testthat", "devtools", "knitr", "microbenchmark", "magrittr", "XML", "memoise", "profr", "ggplot2", "digest", "xtable", "rmarkdown"))'
LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libasan.so.2 R CMD build icd9
LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libasan.so.2 /usr/share/doc/littler/examples/check.r --install-deps --as-cran `ls -t /tmp/icd9.*tar.gz | tail -1`
# R CMD check icd9.*tar.gz
