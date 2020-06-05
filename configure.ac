# Copyright (C) 2014 - 2020  Jack O. Wasey
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

# to regenerate quickly and test:
# # e..g. export ICD_HOME=~/icd
# autoreconf -fv && "${ICD_HOME:-.}/configure" --enable-icd-strip --enable-icd-shutup && cat "${ICD_HOME:-.}/src/Makevars" && grep -v -q '@' "${ICD_HOME:-.}/src/Makevars" || echo "MISSING REPLACEMENT"
AC_PREREQ([2.62])
AC_INIT(icd, [4.0.9.1], [jack@jackwasey.com])
AC_COPYRIGHT([Copyright (C) 2014 - 2020  Jack O. Wasey])
AC_CONFIG_SRCDIR([src])
AC_CONFIG_FILES([src/Makevars])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_MACRO_DIR([inst/m4])

# Care with the following shell environment variables:
# https://www.gnu.org/software/autoconf/manual/autoconf-2.66/html_node/Special-Shell-Variables.html
#m4_ifdef([AC_DISABLE_OPTION_CHECKING], [], [])
: ${R_HOME=$(R RHOME)}
AC_MSG_CHECKING([R_HOME already set])
AS_IF([ test -z "${R_HOME}" ], 
      [
    AC_MSG_RESULT([No])
    AC_MSG_ERROR([Cannot proceed.])
       ], 
       [
AC_MSG_RESULT([${R_HOME}])
        R_BIN=[${R_HOME}/bin/R]
        ]
       )

# Get R's C++11 preferred compiler, flags, std, and use these for C++, and C++11
# but if --configure-vars is set, we want to let this override R's choice, e.g.
# so r-hub can be instructed to skip massive Eigen warning spam!

AC_MSG_CHECKING([R CXX11])
AS_IF([test "${CXX11-unset}" = unset ],
      [
       CXX11=`"${R_BIN}" CMD config CXX11`
  AC_MSG_RESULT([Setting ${CXX11}])
       ], 
       [AC_MSG_RESULT([Using pre-set ${CXX11}])]
       )

AC_MSG_CHECKING([R CXX11STD])
AS_IF([ test "${CXX11STD-unset}" = unset ], 
      [
       CXX11STD=`"${R_BIN}" CMD config CXX11STD`
       AC_MSG_RESULT([Setting R ${CXX11STD}]);
       ], 
       [AC_MSG_RESULT([Using existing ${CXX11STD}])]
       )

AC_MSG_CHECKING([R CXX11FLAGS]);
AS_IF([test "${CXX11FLAGS-unset} = unset" ],
      [
       CXX11FLAGS=`"${R_BIN}" CMD config CXX11FLAGS`
  AC_MSG_RESULT([Setting R ${CXX11FLAGS}])
       ],
       [AC_MSG_RESULT([Using existing ${CXX11FLAGS}])]
       )

# We enforce at least C++11 (as does R4.0.0 now)
CXX="${CXX11}"
CXXFLAGS="${CXX11FLAGS}"
CXXSTD="${CXX11STD}"

CFLAGS=`"${R_BIN}" CMD config CFLAGS`
CPPFLAGS=`"${R_BIN}" CMD config CPPFLAGS`
LIBS="${PKG_LIBS}"

# try to shut up travis/r-hub builds with huge warning burden with default compiler settings (Eigen). Can't seem to do with MAKEFLAGS, or string of R CMD check --install-opts=.....
ICD_SHUTUP_FLAG=""
AC_ARG_VAR([ICD_SHUTUP], 
           [[If ICD_SHUTUP is defined, it tells configure to try to silence all compiler warnings. Alternatively, use --enable-icd-shutup in configure, which will override ICD_SHUTUP, if it is set.]])

AC_ARG_ENABLE([icd_shutup], AS_HELP_STRING([--enable-icd-shutup], [Peaceful compilation]))

AS_IF([test "x$enable_icd_shutup" = "xyes" || test "${ICD_SHUTUP+set}" = set ], [ICD_SHUTUP_FLAG="-w"])

# AC_LANG changes what R_OPENMP and rest of script does. See https://www.gnu.org/software/autoconf/manual/autoconf-2.63/html_node/Language-Choice.html . Affected by CXXFLAGS etc, so these should be set first if needed.
AC_LANG([C++])
# https://www.gnu.org/software/autoconf/manual/autoconf-2.67/html_node/Generic-Compiler-Characteristics.html

# this macro is borrowed from R-devel source, and included in the icd /inst/m4 directory
# --disable-openmp may then be passed to the configure script if needed.
AC_MSG_NOTICE([R core OpenMP m4 macro])
R_OPENMP
AC_MSG_NOTICE([R core OpenMP m4 macro check completed.])

# since some systems have broken OMP libraries
# we also check that the actual package will work
ac_pkg_openmp=no
#if test -n "${OPENMP_CFLAGS}"; then
#  AC_MSG_RESULT([Do have OPENMP_CFLAGS from R_OPENMP])
#fi
if test -n "${OPENMP_CXXFLAGS}"; then
    AC_MSG_CHECKING([OpenMP available, but does it work in a package?])
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([[#include <omp.h>]], [[ omp_get_num_threads(); return 0; ]]) ])
    PKG_CFLAGS="${OPENMP_CFLAGS}" PKG_LIBS="${OPENMP_CFLAGS}" "$R_BIN" CMD SHLIB conftest.c 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && "${R_BIN}" --vanilla -q -e "dyn.load(paste('conftest',.Platform\$dynlib.ext,sep=''))" 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_pkg_openmp=yes;
    AC_MSG_RESULT([OpenMP available and working: ${ac_pkg_openmp}]);
else
    AC_MSG_RESULT([R did not think OpenMP is available]);
fi

AC_MSG_RESULT([icd thinks openmp compilation is possible: ${ac_pkg_openmp}])

  ICD_OPENMP_FLAG=''
AS_IF([test ! "x${ac_pkg_openmp}" = xno], [ ICD_OPENMP_FLAG='$(SHLIB_OPENMP_CXXFLAGS)' ])
AC_SUBST(ICD_OPENMP_FLAG)

DO_KEEP_MAKEVARS="${ICD_KEEP_MAKEVARS}"
AC_ARG_VAR([ICD_KEEP_MAKEVARS], 
           [If defined, the makefile contents will be included in the built binary, to help with debugging])
#Always write the stub as default to not include Makevars content
echo "unsigned char Makevars[[]] = { 0x00 };" > src/makevars.h

AC_ARG_ENABLE([icd_strip], AS_HELP_STRING([--enable-icd-strip], [Enable stripping of binary on Linux and MacOS]))
AS_IF([test "x$enable_icd_strip" != "xno"], [ICD_STRIP=[tools/mk/strip.mk]], [ICD_STRIP=''])
AC_SUBST_FILE([ICD_STRIP])
AC_SUBST(ICD_SHUTUP_FLAG)
AC_OUTPUT
