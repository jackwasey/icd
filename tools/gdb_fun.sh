#!/usr/bin/env bash

# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -eu
IFS=$'\n\t'

MAKEFLAGS="CXXFLAGS=-O0 -g CCFLAGS=-O0 -g CXX=G++ CC=gcc" R CMD INSTALL --vanilla ~/icd9
R -d gdb -e "{$1:-icd:::runSetInt();}"
