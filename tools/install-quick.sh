#!/usr/bin/env bash
"${ICD_HOME?}/tools/build-quick.sh"

(
  MAKEFLAGS=-j$(getconf _NPROCESSORS_ONLN)
  export MAKEFLAGS
  #--use-vanilla \
  #--library="$install_dir" \
  #--no-byte-compile \
  #--configure-args==--disable-openmp \
  #--configure-vars=CXXFLAGS="\"-w\"" \
  R CMD INSTALL --debug \
    --preclean \
    --no-clean-on-error \
    --data-compress=none \
    --install-tests \
    --with-keep.source \
    --no-docs \
    --no-html \
    --no-test-load \
    --no-staged-install \
    "$@" \
    icd*.tar.gz
)
