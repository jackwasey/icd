#!/bin/bash
export ICD_HOME=${ICD_HOME:-"$HOME/icd"}
export VALGRIND_CMD="valgrind --tool=callgrind --simulate-cache=yes --instr-atstart=${INSTR_ATSTART:-no} --separate-threads=no"

function valgrind-cmd () {
	VG_RCODE=${1:-$RCODE}
	RCODE=${VG_RCODE:-message("set RCODE env var or call valgrind-cmd() bash function with the code string")}
	pushd "$ICD_HOME"
	R --vanilla --slave -d "$VALGRIND_CMD" -e "devtools::load_all\(\);${VG_RCODE}"
	popd
}

function valgrind-script () {
	crashscript="${1:-$ICD_HOME/tools/crash-scripts/eigen.R}"
	pushd "$ICD_HOME"
	R --vanilla --slave -d "$VALGRIND_CMD" < "${crashscript}"
	popd
}

