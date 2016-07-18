#!/bin/sh

# set environment here for slow and online tests
OLDWD=`pwd`
cd /tmp
R CMD build --no-build-vignettes ~/icd
ICD_SLOW_TESTS=FALSE R CMD check --no-build-vignettes `ls -t /tmp/icd*.tar.gz | head -1`
cd $OLDWD
