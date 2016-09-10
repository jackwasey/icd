#!/bin/bash
pushd /tmp
R CMD build ~/icd
ICD_SLOW_TESTS=TRUE R CMD check --as-cran "$(ls -t /tmp/icd*.tar.gz | head -1)"
popd
