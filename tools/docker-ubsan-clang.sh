#!/bin/sh
docker pull rocker/r-devel-ubsan-clang && docker run --rm -ti -v $(pwd):/mnt  rocker/r-devel-ubsan-clang sh -c "apt-get update && apt-get install --yes --fix-missing r-cran-xml libxml2-dev && check.r --setwd /mnt --install-deps $1"

