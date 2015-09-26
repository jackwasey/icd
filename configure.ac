AC_PREREQ([2.62])
AC_INIT(icd9)
AC_COPYRIGHT(Copyright (C) 2014 - 2015  Jack O. Wasey)
AC_CONFIG_SRCDIR([src])
AC_CONFIG_HEADERS([src/config.h:src/config.h.in])
AC_CONFIG_FILES([src/Makevars])
AC_CONFIG_MACRO_DIR([m4])

# assuming that calling 'R' will pick the right one, even though we might be running in Rdevel...
# workaround is for R_HOME to be set in advance, preventing the call to the wrong R.
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
R_BIN="${R_HOME}/bin/R"
CC=`"${R_BIN}" CMD config CC`
CPP=`"${R_BIN}" CMD config CPP`
CFLAGS=`"${R_BIN}" CMD config CFLAGS`
CPPFLAGS=`"${R_BIN}" CMD config CPPFLAGS`
LIBS="${PKG_LIBS}"

AC_CHECK_HEADER([string.h])
AC_CHECK_HEADER_STDBOOL
AC_PROG_CXX
AC_PROG_CC
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
AC_CHECK_HEADERS([parallel/algorithm])
AC_LANG_POP([C++])

# more detailed check to actually use parallel glibc in a package:
AC_LANG_PUSH([C++])
GCC_PARALLEL_CXXFLAGS='-D_GLIBCXX_PARALLEL'
ac_pkg_glibc_parallel=no
if test -n "${OPENMP_CFLAGS}"; then
  AC_MSG_CHECKING([OpenMP available, but is glibc parallel mode working via R?])
  AC_LANG_CONFTEST(
  [AC_LANG_PROGRAM([[#include <parallel/algorithm>]], [[ std::vector<int> v; v.push_back(3); v.push_back(2); __gnu_parallel::sort(v.begin(), v.end()); ]])])
  PKG_CFLAGS="${OPENMP_CFLAGS}" PKG_LIBS="${OPENMP_CFLAGS}" PKG_CXXFLAGS="${OPENMP_CXXFLAGS} ${GCC_PARALLEL_CXXFLAGS}" "$R_BIN" CMD SHLIB conftest.c 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && "$R_BIN" --vanilla -q -e "dyn.load(paste('conftest',.Platform\$dynlib.ext,sep=''))" 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_pkg_glibc_parallel=yes
  AC_MSG_RESULT([${ac_pkg_glibc_parallel}])
fi
AC_LANG_POP([C++])

if test "${ac_pkg_glibc_parallel}" = no; then
  GCC_PARALLEL_CXXFLAGS=''
fi
AC_SUBST(GCC_PARALLEL_CXXFLAGS)

# We do not need valgrind for normal operations, but it helps to know whether present on the platform compiling the code, so headers can be included if present (and instrumentation triggers inserted).
# this uses automake, however, and is designed to check newly compiled code: AX_VALGRIND_CHECK
AC_CHECK_HEADERS([valgrind/valgrind.h])

# C++11, autoconf should determine the correct flags
# https://www.gnu.org/software/autoconf-archive/ax_cxx_compile_stdcxx_11.html
AX_CXX_COMPILE_STDCXX_11(noext,optional)

ICD9_CXX11_FLAG=''
if test -n "${HAVE_CXX11}"; then
  AC_MSG_CHECKING([Have C++11, so set Makevars flag for R])
  # ICD9_CXX11_FLAG="AS_ESCAPE([${CXX1XSTD}])"
  # the following 'switch' variable is created by the AC CXX test. This is how R configure gets the flag itself.
  ICD9_CXX11_FLAG=${switch}
fi
AC_SUBST(ICD9_CXX11_FLAG)

AC_OUTPUT
