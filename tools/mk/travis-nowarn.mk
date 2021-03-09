#!/usr/bin/env make
## N.b. += requires GNU make.
# this is mainly for the enormously verbose RcppEigen warnings, which come with any compilation which does LinkingTo RcppEigen, not the package installation itself.

## clang uses unknown-warning-option, gcc uses unknown-warning
icd_shutup_flags = \
                   -Wno-unknown-warning-option \
                   -Wno-unknown-warning \
                   -Wno-unused-parameter \
                   -Wno-unused-variable \
                   -Wno-ignored-attributes \
                   -Wno-cast-function-type \
                   -Wno-unknown-pragmas

CXXFLAGS += $(icd_shutup_flags)
CXX11FLAGS += $(icd_shutup_flags)
CXX14FLAGS += $(icd_shutup_flags)
CXX17FLAGS += $(icd_shutup_flags)
CXX20FLAGS += $(icd_shutup_flags)
CFLAGS += $(icd_shutup_flags)
FCFLAGS += $(icd_shutup_flags)
F77FLAGS += $(icd_shutup_flags)
