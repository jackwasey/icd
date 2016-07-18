#!/bin/sh

# set environment here for slow and online tests
OLDWD=`pwd`
cd /tmp
R CMD build ~/icd
ICD_SLOW_TESTS=TRUE R CMD check --as-cran `ls -t /tmp/icd*.tar.gz | head -1`
cd $OLDWD
