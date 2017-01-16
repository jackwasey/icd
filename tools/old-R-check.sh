#!/bin/bash

# steps for building and checking against old versions of R, using rocker/r-ver:3.1.0 for example

# build tar.gz source package just using
R CMD build --no-build-vignettes --no-manual icd

docker run --rm -ti -v $(pwd):/mnt rocker/r-ver:3.1.0 /bin/bash "/mnt/icd/tools/old-R-check.docker.sh"
