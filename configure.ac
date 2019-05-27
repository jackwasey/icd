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
AC_CONFIG_FILES([src/Makevars])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_MACRO_DIR([inst/m4])

: ${R_HOME=$(R RHOME)}
AC_MSG_CHECKING([R_HOME already set])
if test -z "${R_HOME}"; then
    AC_MSG_RESULT([No])
    AC_MSG_ERROR([Cannot proceed.])
fi
AC_MSG_RESULT([${R_HOME}])
R_BIN="${R_HOME}/bin/R"

# Get R's C++11 preferred compiler, flags, std, and use these for C++, and C++11
# but if --configure-vars is set, we want to let this override R's choice, e.g.
# so r-hub can be instructed to skip massive Eigen warning spam!

AC_MSG_CHECKING([R CXX11])
if test -z "$CXX11"; then
  CXX11=`"${R_HOME}/bin/R" CMD config CXX11`
  AC_MSG_RESULT([Setting ${CXX11}])
else
  AC_MSG_RESULT([Using pre-set ${CXX11}])
fi

AC_MSG_CHECKING([R CXX11STD])
if test -z "$CXX11STD"; then
  CXX11STD=`"${R_HOME}/bin/R" CMD config CXX11STD`
  AC_MSG_RESULT([Setting R ${CXX11STD}])
else
  AC_MSG_RESULT([Using existing ${CXX11STD}])
fi

AC_MSG_CHECKING([R CXX11FLAGS])
if test -z "$CXX11FLAGS"; then
  CXX11FLAGS=`"${R_HOME}/bin/R" CMD config CXX11FLAGS`
  AC_MSG_RESULT([Setting R ${CXX11FLAGS}])
else
  AC_MSG_RESULT([Using existing ${CXX11FLAGS}])
fi

# We enforce at least C++11
CXX=${CXX11}
CXXFLAGS=${CXX11FLAGS}
CXXSTD="${CXX11STD}"

CPP=`"${R_BIN}" CMD config CPP`
CFLAGS=`"${R_BIN}" CMD config CFLAGS`
CPPFLAGS=`"${R_BIN}" CMD config CPPFLAGS`
LIBS="${PKG_LIBS}"

# AC_REQUIRE_CPP

# changes what R_OPENMP and rest of script does:
AC_LANG(C++)

# https://www.gnu.org/software/autoconf/manual/autoconf-2.67/html_node/Generic-Compiler-Characteristics.html

# this macro is borrowed from R-devel source, and included in the icd/tools/m4 directory
R_OPENMP

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

dnl Always write the stub as default to not include Makevars content:
echo "unsigned char Makevars[[]] = { 0x00 };" > src/makevars.h

AC_ARG_ENABLE([makevars],
    AS_HELP_STRING([--enable-makevars],
      [keep ~/.R/Makevars contents in binary for debugging (default is no)]),
    [
    dnl also check that user didn't write --enable-makevars=no
    AS_IF([test "x$enable_makevars" = "xyes"],
    [
      AC_MSG_NOTICE([Enabling Makevars inclusion for debugging])
      AC_MSG_CHECKING([$HOME/.R exists])
      if test -d "$HOME/.R"; then
        AC_MSG_RESULT([yes])
        AC_MSG_CHECKING([$HOME/.R/Makevars exists])
        if test -e "$HOME/.R/Makevars"; then
          AC_MSG_RESULT([yes])
          echo 'unsigned char Makevars[[]] = {' > src/makevars.h
          xxd -i < "$HOME/.R/Makevars" >> src/makevars.h
          # must NULL terminate
          echo ", 0x00 };" >> src/makevars.h
        else
          AC_MSG_RESULT([no])
        fi
      else
        AC_MSG_RESULT([no])
      fi
    ], [
      AC_MSG_NOTICE([Not enabling Makevars inclusion due to --enable-makevars=!yes])
    ])
  ],
  [
    AC_MSG_NOTICE([Not enabling Makevars inclusion])
  ]
)
# put the flag in Makevars
AC_SUBST(ICD_OPENMP_FLAG)
AC_OUTPUT
