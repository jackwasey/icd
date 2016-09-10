#!/bin/bash

# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

MAKEFLAGS="CXXFLAGS=-O0 -g" R CMD INSTALL --vanilla ~/icd9
R -d gdb -e "icd:::runSetInt();"
