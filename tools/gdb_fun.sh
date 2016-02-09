#!/bin/bash
MAKEFLAGS="CXXFLAGS=-O0 -g" R CMD INSTALL --vanilla icd9
R -d gdb -e "library(icd9); icd:::runSetInt();"

