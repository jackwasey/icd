#!/bin/bash
ICD_HOME=${ICD_HOME:-"$HOME/icd"}
VALGRIND_CMD="valgrind --tool=callgrind --simulate-cache=yes --instr-atstart=${INSTR_ATSTART:-no} --separate-threads=no"

function valgrind-cmd () {
	pushd "$ICD_HOME"
	R --vanilla --slave -d "$VALGRIND_CMD" -e \'$RCODE\'
	popd
}

function valgrind-script () {
	crashscript="${1:-$ICD_HOME/tools/crash-scripts/eigen.R}"
	pushd "$ICD_HOME"
	R --vanilla --slave -d "$VALGRIND_CMD" < "${crashscript}"
	popd
}

