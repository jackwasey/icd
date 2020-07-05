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
dnl "${ICD_HOME:-.}/configure" --enable-icd-strip --enable-icd-shutup 
dnl
dnl "${ICD_HOME:-.}/configure"
dnl grep -v -q '@' "${ICD_HOME:-.}/src/Makevars" || echo "MISSING REPLACEMENT"

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
AC_MSG_CHECKING([R_HOME not empty])
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
dnl TODO: if user prefers globally using C++14, C++17, or C++20, then respect
dnl this, since 'icd' should compile fine. All this basically enforces C++11,
dnl whereas we can use C++11 or greater.
AS_IF([test "${CXX11-unset}" = unset ],
	  [
	   CXX11=`"${R_BIN}" CMD config CXX11`
	   AC_MSG_RESULT([Setting ${CXX11}])
	   ], 
	   [AC_MSG_RESULT([Using pre-set ${CXX11}])]
	   )

AC_MSG_CHECKING([R CXX11STD])
AS_IF([test "${CXX11STD-unset}" = unset ], 
	  [
	   CXX11STD=`"${R_BIN}" CMD config CXX11STD`
	   AC_MSG_RESULT([Setting R ${CXX11STD}])
	   ], 
	   [AC_MSG_RESULT([Using existing ${CXX11STD}])]
	   )

AC_MSG_CHECKING([R CXX11FLAGS]);
AS_IF([test "${CXX11FLAGS-unset}" = unset ],
	  [
	   CXX11FLAGS=`"${R_BIN}" CMD config CXX11FLAGS`
	   AC_MSG_RESULT([Setting R ${CXX11FLAGS}])
	   ],
	   [AC_MSG_RESULT([Using existing ${CXX11FLAGS}])]
	   )

# We enforce at least C++11 (as does R4.0.0 now, but which R3.x need explicitly)
CXX="${CXX11}"
CXXFLAGS="${CXX11FLAGS}"
CXXSTD="${CXX11STD}"

CFLAGS=`"${R_BIN}" CMD config CFLAGS`
CPPFLAGS=`"${R_BIN}" CMD config CPPFLAGS`
LIBS="${PKG_LIBS}"

# try to shut up travis/r-hub builds with huge warning burden with default
# compiler settings (Eigen). Can't seem to do with MAKEFLAGS, or string of R CMD
# check --install-opts=.....
ICD_SHUTUP_FLAG=""
AC_ARG_VAR([ICD_SHUTUP], 
		   [[If ICD_SHUTUP is defined, it tells configure to try to silence all compiler warnings. Alternatively, use --enable-icd-shutup in configure, which will override ICD_SHUTUP, if it is set.]])

AC_ARG_ENABLE([icd_shutup], AS_HELP_STRING([--enable-icd-shutup], [Peaceful compilation]))

AS_IF([test "x$enable_icd_shutup" = "xyes" || test "${ICD_SHUTUP+set}" = set ], [ICD_SHUTUP_FLAG="-w"])

# AC_LANG changes what R_OPENMP and rest of script does. See https://www.gnu.org/software/autoconf/manual/autoconf-2.63/html_node/Language-Choice.html . Affected by CXXFLAGS etc, so these should be set first if needed.
AC_LANG([C++])
# https://www.gnu.org/software/autoconf/manual/autoconf-2.67/html_node/Generic-Compiler-Characteristics.html

# the R_OPENMP macro is copied and credited from R core source, and included in
# the icd /inst/m4 directory --disable-openmp may then be passed to the
# configure script if needed. If OpenMP is not available, then we continue
# single-threaded. Eigen may yet use OpenMP with its own tests.
AC_MSG_NOTICE([R core OpenMP m4 macro])
dnl Of note, this check WITHIN the current configure language prefix. AC_LANG([C++]) will therefore check whether it is available for C++ only, and set flags for C++ only?
dnl should already be using C++ here
R_OPENMP
AC_LANG_PUSH([C])
R_OPENMP
AC_LANG_POP
AC_MSG_NOTICE([R core OpenMP m4 macro check completed.])

# since some systems have broken OMP libraries we also check that the actual
# package will work if compiled with these flags.
icd_pkg_openmp_works=no
AC_MSG_NOTICE([Examining what R set for OPENMP_CFLAGS])

AS_IF([ test "x${OPENMP_CFLAGS}" != "x"], [ AC_MSG_NOTICE([[OPENMP_CFLAGS from R_OPENMP is: $(OPENMP_CFLAGS)]]) ], [ AC_MSG_NOTICE([OPENMP_CFLAGS is empty or unset]) ])

#AC_MSG_NOTICE([Examining what R set for OPENMP_CFLAGS done])

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
AS_IF([test ! "x${icd_pkg_openmp_works}" = xno], [ ICD_OPENMP_FLAG='$(SHLIB_OPENMP_CXXFLAGS)' ])

DO_KEEP_MAKEVARS="${ICD_KEEP_MAKEVARS}"
AC_ARG_VAR([ICD_KEEP_MAKEVARS], 
		   [If defined, the makefile contents will be included in the built binary, to help with debugging])
#Always write the stub as default to not include Makevars content
echo "unsigned char Makevars[[]] = { 0x00 };" > src/makevars.h

AC_ARG_ENABLE([icd_strip], AS_HELP_STRING([--enable-icd-strip], [Enable stripping of binary on Linux and MacOS]))
AS_IF([test "x$enable_icd_strip" != "xno"], [ICD_STRIP=[tools/mk/strip.mk]], [ICD_STRIP=''])

dnl This is not supposed to be set by calling environment
ICD_OPTIM_FLAGS=''
dnl AC_ARG_VAR([ICD_OPTIM], 
			   dnl           [[If ICD_OPTIM is defined, it adds useful fairly cross-platform
							  dnl           optimization flags, e.g., -march=native -O3]])

AC_ARG_ENABLE([icd_optim], AS_HELP_STRING([--enable-icd-optim], [Optional addition of more optimized compiler flags. Not default due to CRAN rules, but recommended.]))

AS_IF([test "x$enable_icd_optim" = "xyes" || test "${ICD_OPTIM+set}" = set ], [ICD_OPTIM_FLAGS="-march=native -O3 -fno-unroll-loops -g0 -fomit-frame-pointer"])

AC_SUBST(ICD_OPENMP_FLAG)
AC_SUBST([ICD_OPTIM_FLAGS])
AC_SUBST(ICD_SHUTUP_FLAG)
AC_SUBST_FILE([ICD_STRIP])
AC_OUTPUT
