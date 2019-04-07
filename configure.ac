# Copyright (C) 2014 - 2018  Jack O. Wasey
#
# This file is part of icd.
#
# icd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd. If not, see <http:#www.gnu.org/licenses/>.

AC_PREREQ([2.62])
AC_INIT(icd)
AC_COPYRIGHT(Copyright (C) 2014 - 2019  Jack O. Wasey)
AC_CONFIG_SRCDIR([src])
#AC_CONFIG_HEADERS([src/config.h:src/config.h.in])
AC_CONFIG_FILES([src/Makevars])
AC_CONFIG_MACRO_DIR([tools/m4])

: ${R_HOME=$(R RHOME)}
AC_MSG_CHECKING([R_HOME already set])
if test -z "${R_HOME}"; then
    AC_MSG_RESULT([No])
    AC_MSG_ERROR([Cannot proceed.])
fi
AC_MSG_RESULT([${R_HOME}])
R_BIN="${R_HOME}/bin/R"
CC=`"${R_BIN}" CMD config CC`
CXX11=`"${R_HOME}/bin/R" CMD config CXX11`
CXX11STD=`"${R_HOME}/bin/R" CMD config CXX11STD`
# not sure if i need to enforce next step: R should do this itself, but for configure okay?
# CXX="${CXX11} ${CXX11STD}"
CXX=`"${R_HOME}/bin/R" CMD config CXX`
CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXX11FLAGS`
CXXSTD="$${CXX11STD}"
CPP=`"${R_BIN}" CMD config CPP`
CFLAGS=`"${R_BIN}" CMD config CFLAGS`
CPPFLAGS=`"${R_BIN}" CMD config CPPFLAGS`
LIBS="${PKG_LIBS}"

AC_REQUIRE_CPP

# changes what R_OPENMP does:
AC_LANG(C++)

#AC_CHECK_HEADER([string.h])
#AC_CHECK_HEADER_STDBOOL
#AC_PROG_CC
#AC_PROG_CXX
#AC_TYPE_SIZE_T
#AC_C_INLINE

# https://www.gnu.org/software/autoconf/manual/autoconf-2.67/html_node/Generic-Compiler-Characteristics.html


# this macro is borrowed from R-devel source, and included in the icd/tools/m4 directory
R_OPENMP
#set

# the R openmp macro defines OPENMP_CFLAGS? but not OPENMP_CXXFLAGS? Maybe depends on current AC language?

# now the R openmp macro has already substituted/set OPENMP_CFLAGS, so can't do that again?

# since some systems have broken OMP libraries
# we also check that the actual package will work
ac_pkg_openmp=no
#if test -n "${OPENMP_CFLAGS}"; then
#  AC_MSG_RESULT([Do have OPENMP_CFLAGS from R_OPENMP])
#fi
if test -n "${OPENMP_CXXFLAGS}"; then
  AC_MSG_CHECKING([OpenMP available, but does it work in a package])
  AC_LANG_CONFTEST(
  [AC_LANG_PROGRAM([[#include <omp.h>]], [[ omp_get_num_threads (); return 0; ]])])
  PKG_CFLAGS="${OPENMP_CFLAGS}" PKG_LIBS="${OPENMP_CFLAGS}" "$R_BIN" CMD SHLIB conftest.c 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && "$R_BIN" --vanilla -q -e "dyn.load(paste('conftest',.Platform\$dynlib.ext,sep=''))" 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_pkg_openmp=yes
  AC_MSG_RESULT([OpenMP available and working: ${ac_pkg_openmp}])
else
  AC_MSG_RESULT([R did not think OpenMP is available])
fi

AC_MSG_RESULT([icd thinks openmp compilation possible: ${ac_pkg_openmp}])

# if ${ac_pkg_openmp} = "yes" then we have OMP, otherwise it will be "no"
if test "${ac_pkg_openmp}" = no; then
  ICD_OPENMP_FLAG=''
else
  ICD_OPENMP_FLAG='$(SHLIB_OPENMP_CXXFLAGS)'
fi
# in Makevars.in:
AC_SUBST(ICD_OPENMP_FLAG)

AC_OUTPUT
