#!/usr/bin/env make
icd_debug_flags := -O0 -g3
@echo adding $(icd_debug_flags) to C, Fortran and C++ flags
CXXFLAGS += $(icd_debug_flags)
CXX11FLAGS += $(icd_debug_flags)
CXX14FLAGS += $(icd_debug_flags)
CXX17FLAGS += $(icd_debug_flags)
CXX20FLAGS += $(icd_debug_flags)
CFLAGS += $(icd_debug_flags)
FCFLAGS += $(icd_debug_flags)
F77FLAGS += $(icd_debug_flags)
