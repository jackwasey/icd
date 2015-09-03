#!/bin/sh
# R CMD build icd9
# ./docker-san icd9_1.1.tar.gz

# pre-install binaries for deb packages
# use unstable to get the right development library versions
docker run --rm -ti -v $(pwd):/mnt rocker/r-devel-san sh -c "apt-get update && apt-get --yes dist-upgrade && apt-get --yes install libxml2-dev/unstable libssl-dev/unstable littler r-cran-xml r-cran-ggplot2 r-cran-rcurl r-cran-bitops r-cran-brew r-cran-rcolorbrewer r-cran-rcpp r-cran-dichromat r-cran-munsell r-cran-checkmate r-cran-evaluate r-cran-plyr r-cran-gtable r-cran-reshape2 r-cran-scales r-cran-proto r-cran-catools r-cran-testthat r-cran-memoise r-cran-digest r-cran-xtable && /usr/share/doc/littler/examples/check.r --setwd /mnt --install-deps $1"
