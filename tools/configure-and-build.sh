#!/usr/bin/env bash
set -eu

echo "This is a dirty way to build the library quickly, mainly
just to test that configure script works. Uses -isystem instead
of -I to include without diagnostics from Rcpp and RcppEigen"

echo "${COLOR_LIGHT_BLUE:-}Using icd in ${ICD_HOME?}${COLOR_NC:-}"
declare -a warn_flags
warn_flags=("")
# this process still pickups up on ~/.R/Makevars

(
set -x
R_HOME="$(R RHOME)"
CXX_STD=CXX17

#PKG_CXXFLAGS="-w $(Rscript -e 'Rcpp:::CxxFlags()') -I ${ICD_HOME}/inst/include -I ${ICD_HOME}/inst/include/eigen-submodule"
PKG_CXXFLAGS="${warn_flags[*]} $(Rscript -e 'Rcpp:::CxxFlags()' | sed 's/-I/-isystem/') $(Rscript -e 'RcppEigen:::CxxFlags()' | sed 's/-I/-isystem/')"
PKG_CXX11FLAGS="${PKG_CXXFLAGS}"
PKG_CFLAGS="${warn_flags[*]}"
# CXX="ccache g++"
# CC="ccache gcc"
CXX=$("${R_HOME}/bin/R" CMD config CXX)
CC=$("${R_HOME}/bin/R" CMD config CC)
export MAKEFLAGS=-j8
export PKG_CXXFLAGS PKG_CFLAGS CC CXX CXX_STD 
# also need CXX17FLAGS, or is this just for _additional_ flags for that
# compiler?
export PATH="/usr/lib/ccache:${PATH}"

cd "${ICD_HOME?}"
autoreconf
./configure --enable-icd-makevars --enable-icd-shutup  --disable-icd-strip
R CMD SHLIB --preclean --output=/tmp/icd.shlib src/*.cpp src/*.c
)
