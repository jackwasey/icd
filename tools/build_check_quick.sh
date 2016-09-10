#!/bin/bash

# set environment here for slow and online tests
pushd /tmp
R CMD build --no-build-vignettes ~/icd
ICD_SLOW_TESTS=FALSE R CMD check --no-build-vignettes "$(ls -t /tmp/icd*.tar.gz | head -1)"
popd
