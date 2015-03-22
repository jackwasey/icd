#!/bin/sh
# e.g.
# R CMD build icd9
# ./ubsan-docker icd9_1.1.tar.gz
docker run --rm -ti -v $(pwd):/mnt  rocker/r-devel-ubsan-clang sh -c "apt-get update && apt-get install --yes --fix-missing r-cran-xml libxml2-dev && check.r --as-cran --setwd /mnt --install-deps $1"

