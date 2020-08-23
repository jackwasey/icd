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

dnl to regenerate quickly and test: # # e.g. export ICD_HOME=~/icd
dnl autoreconf -fv
dnl "${ICD_HOME:-.}/configure" --enable-icd-shutup 
dnl
dnl "${ICD_HOME:-.}/configure"
dnl grep -v -q '@' "${ICD_HOME:-.}/src/Makevars" || echo "MISSING REPLACEMENT"

AC_PREREQ([2.69])
AC_INIT([icd],[4.0.9.1],[jack@jackwasey.com])
AC_COPYRIGHT([Copyright (C) 2014 - 2020  Jack O. Wasey])
AC_CONFIG_SRCDIR([src])
AC_CONFIG_FILES([src/Makevars])
AC_CONFIG_HEADERS([src/config.h])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_MACRO_DIR([inst/m4])

# Care with the following shell environment variables:
# https://www.gnu.org/software/autoconf/manual/autoconf-2.66/html_node/Special-Shell-Variables.html
#m4_ifdef([AC_DISABLE_OPTION_CHECKING], [], [])

: ${R_HOME:=`R RHOME`}
AC_MSG_CHECKING([R_HOME is still not empty])
AS_IF([ test -d "${R_HOME}" ], 
	  [
	   AC_MSG_RESULT([${R_HOME}])
	   ], 
	   [
		AC_MSG_RESULT([No])
		AC_MSG_ERROR([Cannot proceed.])
		]
	   )

: ${R_BIN:=${R_HOME}/bin/R}
: ${R_CXX:=`"${R_BIN}" CMD config CXX`}
: ${R_CXXFLAGS:=`"${R_BIN}" CMD config CXXFLAGS`}
: ${R_CXX11:=`"${R_BIN}" CMD config CXX11`}
: ${R_CXX11FLAGS:=`"${R_BIN}" CMD config CXX11FLAGS`}
: ${R_CXX14:=`"${R_BIN}" CMD config CXX14`}
: ${R_CXX14FLAGS:=`"${R_BIN}" CMD config CXX14FLAGS`}
: ${R_CXX17:=`"${R_BIN}" CMD config CXX16`}
: ${R_CXX17FLAGS:=`"${R_BIN}" CMD config CXX17FLAGS`}
: ${R_CXX20:=`"${R_BIN}" CMD config CXX20`}
: ${R_CXX20FLAGS:=`"${R_BIN}" CMD config CXX20FLAGS`}
: ${R_CFLAGS:=`"${R_BIN}" CMD config CFLAGS`}
: ${R_CPPFLAGS:=`"${R_BIN}" CMD config CPPFLAGS`}

if ! "${R_BIN}" CMD config CXX11STD 2>/dev/null >/dev/null; then AC_MSG_ERROR([icd requires a compiler with at least C++11 support]); fi
if "${R_BIN}" CMD config CXX14STD 2>/dev/null >/dev/null; then HAVE_CXX14=1; fi
if "${R_BIN}" CMD config CXX17STD 2>/dev/null >/dev/null; then HAVE_CXX17=1; fi
if "${R_BIN}" CMD config CXX20STD 2>/dev/null >/dev/null; then HAVE_CXX20=1; fi

AC_MSG_CHECKING([C++ standard]);
if test "x$CXXSTD" = "x"; then 
	if test "x$HAVE_CXX14" != "x"; then CXXSTD='CXX14'; CXX="${R_CXX14}"; fi
	if test "x$HAVE_CXX17" != "x"; then CXXSTD='CXX17'; CXX="${R_CXX17}";fi
	if test "x$HAVE_CXX20" != "x"; then CXXSTD='CXX20'; CXX="${R_CXX20}";fi
	AC_MSG_RESULT([latest R toolchain CXXSTD available is ${CXXSTD}]);
else
	AC_MSG_RESULT([user set CXXSTD to ${CXXSTD}]);
fi

# Get R's C++11 preferred compiler, flags, std, and use these for C++, and C++11
# but if --configure-vars is set, we want to let this override R's choice, e.g.
# so r-hub can be instructed to skip massive Eigen warning spam!

dnl AC_MSG_CHECKING([R CFLAGS]);
dnl AS_IF([test "${CFLAGS-unset}" = unset ], [AC_MSG_RESULT([${R_CFLAGS}])])
dnl AS_IF([test "${CXX11FLAGS-unset}" = unset ], [AC_MSG_RESULT([Will use from R: ${R_CXX11FLAGS}])], [AC_MSG_RESULT([already set to: ${CXX11FLAGS}])])
dnl AS_IF([test "${PKG_CXXFLAGS-unset}" = unset ], [AC_MSG_RESULT([PKG_CXXFLAGS not being used.])], [AC_MSG_RESULT([${PKGCXXFLAGS])])

# We enforce at least C++11 (as does R4.0.0 now, but which R3.x need explicitly)
RCPP_CXXFLAGS=[`Rscript -e 'Rcpp:::CxxFlags()'`]
#RCPPEIGEN_CXXFLAGS=[`Rscript -e 'RcppEigen:::CxxFlags()'`]
RCPPEIGEN_CXXFLAGS=[-I`Rscript -e 'cat(system.file("include", package = "RcppEigen"))'`]
AC_MSG_NOTICE([Got Rcpp and RcppEigen C++ flags])
dnl TODO: consider -I to -isystem switch, at least for debug
EIGEN_QUIET_FLAGS=[-DEIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS]


CFLAGS="${R_CFLAGS} ${CFLAGS} ${PKG_CFLAGS}"
CPPFLAGS="${R_CPPFLAGS} ${RCPP_CXXFLAGS} ${RCPPEIGEN_CXXFLAGS} ${CPPFLAGS} ${PKG_CPPFLAGS}"
CXXFLAGS="${R_CXXFLAGS} ${RCPP_CXXFLAGS} ${RCPPEIGEN_CXXFLAGS} ${CXXFLAGS} ${PKG_CXXFLAGS}"
CXX11FLAGS="${R_CXX11FLAGS} ${RCPP_CXXFLAGS} ${RCPPEIGEN_CXXFLAGS} ${CXX11FLAGS} ${PKG_CXX11FLAGS}" 
CXX14FLAGS="${R_CXX14FLAGS} ${RCPP_CXXFLAGS} ${RCPPEIGEN_CXXFLAGS} ${CXX14FLAGS} ${PKG_CXX14FLAGS}" 
CXX17FLAGS="${R_CXX17FLAGS} ${RCPP_CXXFLAGS} ${RCPPEIGEN_CXXFLAGS} ${CXX17FLAGS} ${PKG_CXX17FLAGS}" 
CXX20FLAGS="${R_CXX20FLAGS} ${RCPP_CXXFLAGS} ${RCPPEIGEN_CXXFLAGS} ${CXX20FLAGS} ${PKG_CXX20FLAGS}" 
AC_MSG_NOTICE([Final CXXFLAGS is ${CXXFLAGS}])
AC_MSG_NOTICE([Final CXX11FLAGS is ${CXX11FLAGS}])

LIBS="${PKG_LIBS}"

# try to shut up travis/r-hub builds with huge warning burden with default
# compiler settings (Eigen). Can't seem to do with MAKEFLAGS, or string of R CMD
# check --install-opts=.....
ICD_SHUTUP_FLAG=""
AC_ARG_VAR([ICD_SHUTUP], 
		   [[If ICD_SHUTUP is defined, it tells configure to try to silence all compiler warnings. Mainly this is due to massive verbosity of RcppEigen (and somewhat Rcpp itself) when building on CI platforms, making actual warnings impossible to uncover. Alternatively, use --enable-icd-shutup in configure, which will override ICD_SHUTUP, if it is set.]])

AC_ARG_ENABLE([icd_shutup], AS_HELP_STRING([--enable-icd-shutup], [Peaceful compilation]))

AS_IF([test "x$enable_icd_shutup" = "xyes" || test "${ICD_SHUTUP+set}" = set ], [ICD_SHUTUP_FLAG="-w"])

# AC_LANG changes what R_OPENMP and rest of script does. See
# https://www.gnu.org/software/autoconf/manual/autoconf-2.69/html_node/Language-Choice.html
# . Affected by CXXFLAGS etc, so these should be set first if needed. Since Eigen
# (and formerly icd itself) benefitted from OpenMP, the check should be done in a
# C++ context.
AC_LANG([C++])
# https://www.gnu.org/software/autoconf/manual/autoconf-2.67/html_node/Generic-Compiler-Characteristics.html

AC_ARG_VAR([ICD_DEBUG], [enable icd debugging for compilation])
: ${ICD_DEBUG+-DICD_DEBUG}
dnl AS_VAR_APPEND([CPPFLAGS], [" -DICD_DEBUG "]) # may be useful. PREPEND? m4_append
dnl May be useful: AC_C_BIGENDIAN for bit processing if needed

# I would like to use smaller unit for storing matrix product, ideall bool, but
# unsigned char is something. Some things can be assumed by R's C version
# dependency. This is done for C++ using C check, as described in documentation.
dnl AC_C_CHAR_UNSIGNED

# the R_OPENMP macro is copied and credited from R core source, and included in
# the icd /inst/m4 directory --disable-openmp may then be passed to the
# configure script if needed. If OpenMP is not available, then we continue
# single-threaded. Eigen may yet use OpenMP with its own tests.
AC_MSG_NOTICE([R core OpenMP m4 macro])
dnl Of note, this check WITHIN the current configure language prefix. AC_LANG([C++]) will therefore check whether it is available for C++ only, and set flags for C++ only?
dnl should already be using C++ here
dnl AC_LANG_PUSH([C])
dnl R_OPENMP
dnl AC_OPENMP
dnl AC_LANG_POP
# Why not AC_OPENMP? And why not do this in C++?
R_OPENMP
AC_MSG_NOTICE([R core OpenMP m4 macro check completed.])
dnl AC_OPENMP
dnl AC_MSG_NOTICE([autoconf OpenMP check completed.])

# since some systems have broken OMP libraries we also check that the actual
# package will work if compiled with these flags.
icd_pkg_openmp_works=no
AC_MSG_NOTICE([Examining what R set for OPENMP_CFLAGS])

AS_IF([ test "x${OPENMP_CFLAGS}" != "x"], [ AC_MSG_NOTICE([[OPENMP_CFLAGS from R_OPENMP is: ${OPENMP_CFLAGS}]]) ], [ AC_MSG_NOTICE([OPENMP_CFLAGS is empty or unset]) ])

AC_MSG_NOTICE([Examining what R set for OPENMP_CFLAGS done])

AS_IF([test "x${OPENMP_CXXFLAGS}" != "x" ], [
	   AC_MSG_CHECKING([OpenMP available, but does it work in a package?])
	   AC_LANG_CONFTEST([
						 AC_LANG_PROGRAM([[#include <omp.h>
										  ]],
										  [[ omp_get_num_threads(); return 0; ]])
						 ])
	   AC_MSG_NOTICE([Can at least include the omp.h header and get thread count])
	   PKG_CFLAGS="${OPENMP_CFLAGS}" \
		   PKG_LIBS="${OPENMP_CFLAGS}" \
		   "$R_BIN" CMD SHLIB conftest.c 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && \
		   "${R_BIN}" --vanilla -q -e "dyn.load(paste0('conftest',.Platform\$dynlib.ext))" 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && \
		   icd_pkg_openmp_works=yes;

	   AC_MSG_RESULT([OpenMP available and working with R toolchain])
	   ],
	   [
		AC_MSG_RESULT([R did not think OpenMP is available]);
		]
	   )

AC_MSG_RESULT([icd thinks openmp compilation is possible: ${icd_pkg_openmp_works}])

ICD_OPENMP_FLAG=''
AS_IF([test ! "x${icd_pkg_openmp_works}" = xno], [ ICD_OPENMP_FLAG='${SHLIB_OPENMP_CXXFLAGS}' ])

dnl If ICD_OPTIM is defined, it adds useful fairly cross-platform
dnl optimization flags, e.g., -march=native -O3 but CRAN probably wouldn't like this
ICD_OPTIM_FLAGS=''
dnl AC_ARG_VAR([ICD_OPTIM], [[]])

AC_ARG_ENABLE([icd_optim], [AS_HELP_STRING([--enable-icd-optim], [Optional addition of more optimized compiler flags. Not default due to CRAN rules, but recommended.])])

AS_IF([ test "x$enable_icd_optim" = "xyes" || test "${ICD_OPTIM+set}" = set ],
	  [ ICD_OPTIM_FLAGS="-march=native -O3 -g0 -fomit-frame-pointer" ])

# --icd-optim-info will show a little information on steps performed in optimizing the problem to be solved.

AC_ARG_VAR([ICD_OPTIM_INFO], [occasional info when comorbidity problem optimization was possible (default is no)])
: ${ICD_OPTIM_INFO=-DICD_OPTIM_INFO}

AC_CHECK_HEADERS([Rcpp.h])
AC_CHECK_HEADERS([RcppEigen.h])
AC_CHECK_HEADERS([Eigen/Eigen])
AC_CHECK_HEADERS([Eigen/Sparse])
#AC_CHECK_HEADERS([testthat.h])
AC_CHECK_HEADERS([functional])
AC_CHECK_HEADERS([unordered_map])
AC_CHECK_HEADERS([unordered_set])
AC_CHECK_HEADERS([chrono])
AC_CHECK_HEADERS([ratio])
AC_CHECK_HEADERS([ctime])

AC_SUBST(CFLAGS)
AC_SUBST(CXXFLAGS)
AC_SUBST(CXXSTD)
AC_SUBST(ICD_OPENMP_FLAG)
AC_SUBST([ICD_OPTIM_FLAGS])
AC_SUBST([ICD_OPTIM_INFO_FLAG])
AC_SUBST(ICD_SHUTUP_FLAG)
AC_OUTPUT
