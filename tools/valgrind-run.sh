#!/bin/bash
# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -uo pipefail
IFS=$'\n\t'

R_READY="message(\"ready to valgrind\")"
# thread-related valgrind runs
# http://www.valgrind.org/docs/manual/manual-core.html#manual-core.pthreads
# Valgrind may not work well with libomp (most people use libgomp)

VG_TOOL="callgrind"
INSTR_ATSTART="no"
RSNIP="x <- icd:::generate_random_pts(1000000); invisible(icd_comorbid_quan_deyo(x))"

R_CG_START=""
R_CG_STOP=""
IAS_ARG=""
if [ $VG_TOOL = "callgrind" ]; then
  R_CG_START="icd:::valgrindCallgrindStart()"
  R_CG_STOP="icd:::valgrindCallgrindStop()"
  IAS_ARG="--instr-atstart=$INSTR_ATSTART"
fi

# VG_TOOL="helgrind"
VG="valgrind --tool=${VG_TOOL} ${IAS_ARG:-}"
R_CODE_DEFAULT="library(icd); $R_READY; $R_CG_START; $RSNIP; $R_CG_STOP"
R_CODE=${RCODE:-$R_CODE_DEFAULT}
# run data race detector or helgrind
R CMD INSTALL --no-help --no-docs --no-byte-compile ~/icd && R --vanilla --slave -d "${VG}" -e "${R_CODE}"

# run with helgrind http://valgrind.org/docs/manual/hg-manual.html "Helgrind is
# a Valgrind tool for detecting synchronisation errors in C, C++ and Fortran
# programs that use the POSIX pthreads threading primitives."
