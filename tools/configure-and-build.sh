#!/usr/bin/env bash
set -eux

echo "This is a dirty, not recommended way to build the library quickly, mainly just to test that configure script works."

declare R_HOME
[ "x${R_HOME-}" = x ] && R_HOME="$(R RHOME)"


CXX_STD=CXX11

PKG_CXXFLAGS="$(Rscript -e 'Rcpp:::CxxFlags()') $(Rscript -e 'RcppEigen:::CxxFlags()')"
PKG_CFLAGS="-w"
CXX=$("${R_HOME}/bin/R" CMD config CXX)
CC=$("${R_HOME}/bin/R" CMD config CC)
export PKG_CXXFLAGS PKG_CFLAGS CC CXX CXX_STD
export PATH="/usr/bin/ccache:${PATH}"

cd "${ICD_HOME?}"
autoreconf
./configure
R CMD SHLIB --preclean --output=/tmp/icd.shlib src/*.cpp src/*.c
