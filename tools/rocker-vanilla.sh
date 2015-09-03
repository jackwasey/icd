#!/bin/sh
#docker run --rm -ti -v $(pwd):/mnt  rocker/r-base sh -c "apt-get update && apt-get install --yes --fix-missing littler r-cran-xml libxml2-dev/unstable libcurl4-gnutls-dev && /usr/share/doc/littler/examples/check.r --setwd /mnt --install-deps $1"
# no benefit of installing deb packages if check.r reinstalls them from ssource
docker run --rm -ti -v $(pwd):/mnt  rocker/r-base sh -c "apt-get update && apt-get install --yes --fix-missing littler libxml2-dev/unstable libcurl4-gnutls-dev libssl-dev && /usr/share/doc/littler/examples/check.r --setwd /mnt --install-deps $1"
