AC_PREREQ([2.62])
AC_INIT(icd9)
AC_CONFIG_SRCDIR([src])
AC_CONFIG_HEADER([src/config.h:src/config.h.in])
AC_CONFIG_FILES([src/Makevars])

AC_CONFIG_MACRO_DIR([m4])

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

# following CXX and CXXFLAGS as recommended by Prof Ripley.
CXX=`"${R_HOME}/bin/R" CMD config CXX1X`
CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXX1XFLAGS`

AC_LANG_PUSH(C++)
AC_OPENMP
AC_LANG_POP(C++)

# Checks for programs
AC_PROG_CXX
AC_PROG_CC

# Check for OpenMP, again with thanks to Prof Ripley for tweak
AC_LANG_PUSH(C++)
AC_OPENMP
AC_LANG_POP(C++)

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
  OPENMP_CFLAGS=''
fi
AC_SUBST(OPENMP_CFLAGS)

# other checks recommended by autoscan
AC_CHECK_HEADERS([string.h])
AC_CHECK_HEADER_STDBOOL
AC_PROG_CXX
AC_TYPE_SIZE_T
AC_C_INLINE

# C++11, autoconf should determine the correct flags
# https://www.gnu.org/software/autoconf-archive/ax_cxx_compile_stdcxx_11.html
AX_CXX_COMPILE_STDCXX_11(noext,optional)

ICD9_CXX11=''
ICD9_CXX11_DEF=''
if test -n "${HAVE_CXX11}"; then
  AC_MSG_CHECKING([Have C++11, so set Makevars flag for R])
  # ICD9_CXX11="AS_ESCAPE([${CXX1XSTD}])"
  # the following 'switch' variable is created by the AC CXX test. This is how R configure gets the flag itself.
  ICD9_CXX11=${switch}
  ICD9_CXX11_DEF=""
fi
AC_SUBST(ICD9_CXX11)
AC_SUBST(ICD9_CXX11_DEF)

AC_OUTPUT
