#!/bin/bash

# thread-related valgrind runs
# http://www.valgrind.org/docs/manual/manual-core.html#manual-core.pthreads
# Valgrind may not work well with libomp (most people use libgomp)

RCODE="devnull <- icd9ComorbidQuanDeyo(icd9:::randomPatients(1000000))"
INSTR_ATSTART="no"

# run data race detector.
R --vanilla --slave -d "valgrind --tool=drd --instr-atstart=$INSTR_ATSTART" -e "library(icd9); message(\"ready to valgrind\"); $RCODE"

# run with helgrind
# http://valgrind.org/docs/manual/hg-manual.html
# "Helgrind is a Valgrind tool for detecting synchronisation errors in C, C++ and Fortran programs that use the POSIX pthreads threading primitives."
R --vanilla --slave -d "valgrind --tool=helgrind" -e "library(icd9); message(\"ready to valgrind\"); $RCODE"

