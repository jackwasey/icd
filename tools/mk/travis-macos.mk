#!/usr/bin/env make
## N.b. += requires GNU make.
# this is mainly for the enormously verbose RcppEigen warnings, which come with any compilation which does LinkingTo RcppEigen, not the package installation itself.
icd_osx_comp_ver = 10
icd_osx_cxx = /usr/local/bin/g++
icd_osx_cc = /usr/local/bin/gcc
icd_osx_fc = $(shell sh -c "command -v gfortran || command -v gfortran-$(icd_osx_comp_ver) || echo 'cannot find gfortran!' >&2" )

CXX = $(icd_osx_cxx)-$(icd_osx_comp_ver)
CXX11 = $(CXX)
CXX14 = $(CXX)
CXX17 = $(CXX)
CXX20 = $(CXX)
CC = $(icd_osx_cc)-$(icd_osx_comp_ver)
FC = $(icd_osx_fc)
F77 = $(FC)
