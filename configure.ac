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
AC_COPYRIGHT(Copyright (C) 2014 - 2018  Jack O. Wasey)
AC_CONFIG_SRCDIR([src])
AC_CONFIG_HEADERS([src/config.h:src/config.h.in])
AC_CONFIG_FILES([src/Makevars])
AC_CONFIG_MACRO_DIR([tools/m4])

echo "Checking whether R_HOME already set? R_HOME = ${R_HOME}"
: ${R_HOME:=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
R_BIN="${R_HOME}/bin/R"
CC=`"${R_BIN}" CMD config CC`
CXX=`"${R_BIN}" CMD config CXX`
CPP=`"${R_BIN}" CMD config CPP`
CFLAGS=`"${R_BIN}" CMD config CFLAGS`
CPPFLAGS=`"${R_BIN}" CMD config CPPFLAGS`
CXXFLAGS=`"${R_BIN}" CMD config CXXFLAGS`
LIBS="${PKG_LIBS}"

AC_CHECK_HEADER([string.h])
AC_CHECK_HEADER_STDBOOL
AC_PROG_CC
AC_PROG_CXX
AC_TYPE_SIZE_T
AC_C_INLINE

# this macro is borrowed from R-devel source, and included in the m4 directory
# possibly do this surrounded by push C++ language, but it is probably the same for C and C++ anyway, so I'll just use OPENMP_CFLAGS in Makefile.in
R_OPENMP

# now the R openmp macro has already substituted OPENMP_CFLAGS, so can't do that again

# since some systems have broken OMP libraries
# we also check that the actual package will work
ac_pkg_openmp=no
if test -n "${OPENMP_CFLAGS}"; then
  AC_MSG_CHECKING([OpenMP available, but does it work in a package])
  AC_LANG_CONFTEST(
  [AC_LANG_PROGRAM([[#include <omp.h>]], [[ return omp_get_num_threads (); ]])])
  PKG_CFLAGS="${OPENMP_CFLAGS}" PKG_LIBS="${OPENMP_CFLAGS}" "$R_BIN" CMD SHLIB conftest.c 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && "$R_BIN" --vanilla -q -e "dyn.load(paste('conftest',.Platform\$dynlib.ext,sep=''))" 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_pkg_openmp=yes
  AC_MSG_RESULT([${ac_pkg_openmp}])
fi

# if ${ac_pkg_openmp} = "yes" then we have OMP, otherwise it will be "no"
if test "${ac_pkg_openmp}" = no; then
  OPENMP_CFLAGS_IN_PKG=''
else
  OPENMP_CFLAGS_IN_PKG=${OPENMP_CFLAGS}
  AC_DEFINE(HAVE_R_OPENMP, 1, "Whether OpenMP is available in R environment.")
fi
AC_SUBST(OPENMP_CFLAGS_IN_PKG)

# this only works in C++, since it is a C++ system header...
AC_LANG_PUSH([C++])

# We do not need valgrind for normal operations, but it helps to know whether present on the platform compiling the code, so headers can be included if present (and instrumentation triggers inserted).
# this uses automake, however, and is designed to check newly compiled code: AX_VALGRIND_CHECK
AC_CHECK_HEADERS([valgrind/valgrind.h])

AC_MSG_NOTICE([getting path to test for testthat Catch headers])
TESTTHAT_INCLUDE_DIR=`"${R_BIN}" --vanilla --slave -q -e "suppressWarnings(try(cat(tools::file_path_as_absolute(base::system.file(package='testthat','include','testthat',mustWork=FALSE))),silent=TRUE))"`
# maybe a better way, but I need to have the testthat package include dir in the pre-processor include directories:
CPPFLAGS="$CPPFLAGS -I$TESTTHAT_INCLUDE_DIR"

AC_LANG_PUSH([C++])
# this gives a config var with full path in it when built via check: AC_CHECK_HEADERS([testthat.h])
# these don't work:
## AC_CHECK_HEADERS([testthat])
## AC_CHECK_HEADERS([testthat.h])

AC_CHECK_HEADER(testthat.h, [AC_DEFINE(HAVE_TESTTHAT_H, 1, [define as 1 if testthat catch available])],
  [AC_MSG_NOTICE([testthat headers for Catch tests not found])])

AC_LANG_POP([C++])
# end testthat test

# TODO: if a debug flag is set, then update .Rbuildignore to NOT exclude _alt.cpp etc files

AC_OUTPUT
