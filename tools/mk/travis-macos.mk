#!/usr/bin/env make

icd_osx_comp_ver = -9
icd_osx_cxx = /usr/local/bin/g++
icd_osx_cc = /usr/local/bin/gcc
icd_osx_fc_ver = $(shell sh -c "command -v gfortran || command -v gfortran$(icd_osx_comp_ver) || echo 'cannot find gfortran!' >&2" )

# try some prophylactic debugging:
#@echo Old CC, etc are $(CXX) $(CXX11) $(CXX14) $(CXX17) $(CXX20) $(CC) $(FC) $(F77)

CXX = $(icd_osx_cxx)$(icd_osx_comp_ver)
CXX11 = $(CXX)
CXX14 = $(CXX)
CXX17 = $(CXX)
CXX20 = $(CXX)
CC = $(icd_osx_cc)$(icd_osx_comp_ver)
FC = $(icd_osx_fc_ver)
F77 = $(FC)
