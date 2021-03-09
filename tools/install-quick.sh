#!/usr/bin/env bash
"${ICD_HOME?}/tools/build-quick.sh"

(
MAKEFLAGS=-j$(getconf _NPROCESSORS_ONLN)
export MAKEFLAGS
R CMD INSTALL --debug \
    --data-compress=none \
    --configure-vars="CXX20FLAGS=-O0 CXX17FLAGS=-O0 CXX14FLAGS=-O0 CXX11FLAGS=-O0 CXXFLAGS=-O0 CFLAGS=-O0" \
    --install-tests \
    --no-clean-on-error \
    --no-docs \
    --no-html \
    --no-test-load \
    --no-staged-install \
    icd*.tar.gz

    #    --library="$install_dir" \
    #    --no-byte-compile \
    #    --no-resave-data \
    #    --no-build-vignettes \
    #    --no-clean-on-error \
    #    --no-docs \
)
