#!/usr/bin/env make
## N.b. += requires GNU make.
# this is mainly for the enormously verbose RcppEigen warnings, which come with any compilation which does LinkingTo RcppEigen, not the package installation itself.

icd_debug_flags := -O0 -g3 -Wall -Wextra -pedantic

CXXFLAGS += $(icd_debug_flags)
CXX11FLAGS += $(icd_debug_flags)
CXX14FLAGS += $(icd_debug_flags)
CXX17FLAGS += $(icd_debug_flags)
CXX20FLAGS += $(icd_debug_flags)
CFLAGS += $(icd_debug_flags)
FCFLAGS += $(icd_debug_flags)
F77FLAGS += $(icd_debug_flags)
