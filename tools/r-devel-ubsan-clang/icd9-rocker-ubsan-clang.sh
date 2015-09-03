#!/bin/sh
# R CMD build icd9
# ./docker-usban-clang icd9_1.2.1.tar.gz

docker run --rm -ti -v $(pwd):/mnt icd9-rocker-ubsan-clang /usr/share/doc/littler/examples/check.r --setwd /mnt --install-deps $1
