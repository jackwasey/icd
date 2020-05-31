#!/usr/bin/env bash
set -eu

echo "This is a dirty, not recommended way to build the library quickly, mainly just to test that configure script works."

set -x

CXX_STD=CXX17

PKG_CXXFLAGS="-w $(Rscript -e 'Rcpp:::CxxFlags()') $(Rscript -e 'RcppEigen:::CxxFlags()')"
PKG_CFLAGS="-w"
# CXX="ccache g++"
# CC="ccache gcc"
CXX=$("${R_HOME}/bin/R" CMD config CXX)
CC=$("${R_HOME}/bin/R" CMD config CC)
#MAKEFLAGS=-j8
export PKG_CXXFLAGS PKG_CFLAGS CC CXX CXX_STD 
export PATH="/usr/bin/ccache:${PATH}"

cd "${ICD_HOME?}"
autoreconf
./configure --enable-icd-makevars --enable-icd-shutup  --enable-icd-strip
R CMD SHLIB --preclean --output=/tmp/icd.shlib src/*.cpp src/*.c
