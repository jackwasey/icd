#!/bin/bash
#shellcheck disable=SC2012
set -euo pipefail
IFS=$'\n\t'
whereami="$(dirname "${BASH_SOURCE[0]}")"
#shellcheck source=find-icd-home.sh
source "${whereami}/find-icd-home.sh"
# ICD_HOME=$(find_icd_home)
tmpd=$(mktemp -d /tmp/icdcheckcranplusjit.XXXXXXXXXXX)
function finish {
    #	  rm -rf "$tmpd"
    echo "Finished with $tmpd"
}
trap finish EXIT
#rsync -r --exclude=".git" "${ICD_HOME:-$HOME/rprojects/icd}" "$tmpd"
cd "$tmpd"
# build with standard release options, i.e. compacting vignettes.
"${whereami}"/build-full.sh
# for all environment variable options see here:
# https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Tools
#R_MAKEVARS_USER="$HOME/.R/Makevars.clang" \
MAKEFLAGS=-j$(getconf _NPROCESSORS_ONLN) \
    _R_CHECK_ALL_NON_ISO_C_=TRUE \
    _R_CHECK_ALWAYS_LOG_VIGNETTE_OUTPUT_=TRUE \
    _R_CHECK_CODE_ASSIGN_TO_GLOBALENV_=TRUE \
    _R_CHECK_CODE_ATTACH_=TRUE \
    _R_CHECK_CODE_DATA_INTO_GLOBALENV_=TRUE \
    _R_CHECK_CODE_USAGE_VIA_NAMESPACES_=TRUE \
    _R_CHECK_CODETOOLS_PROFILE_="suppressLocalUnused=FALSE" \
    _R_CHECK_COMPILATION_FLAGS_=FALSE \
    _R_CHECK_CRAN_INCOMING_=FALSE \
    _R_CHECK_CRAN_INCOMING_REMOTE_=FALSE \
    _R_CHECK_DEPRECATED_DEFUNCT_=TRUE \
    _R_CHECK_DOC_SIZES2_=TRUE \
    _R_CHECK_DOT_FIRSTLIB_=TRUE \
    _R_CHECK_EXECUTABLES_EXCLUSIONS_=FALSE \
    _R_CHECK_EXIT_ON_FIRST_ERROR_=FALSE \
    _R_CHECK_INSTALL_DEPENDS_=TRUE \
    _R_CHECK_NATIVE_ROUTINE_REGISTRATION_=TRUE \
    _R_CHECK_NO_RECOMMENDED_=TRUE \
    _R_CHECK_NO_STOP_ON_TEST_ERROR_=TRUE \
    _R_CHECK_OVERWRITE_REGISTERED_S3_METHODS_=TRUE \
    _R_CHECK_PRAGMAS_=TRUE \
    _R_CHECK_R_DEPENDS_=warn \
    _R_CHECK_R_ON_PATH_=TRUE \
    _R_CHECK_RD_EXAMPLES_T_AND_F_=TRUE \
    _R_CHECK_RD_LINE_WIDTHS_=true \
    _R_CHECK_REPLACING_IMPORTS_=TRUE \
    _R_CHECK_S3_METHODS_NOT_REGISTERED_=TRUE \
    _R_CHECK_SCREEN_DEVICE_=stop \
    _R_CHECK_SERIALIZATION_=TRUE \
    _R_CHECK_SUGGESTS_ONLY_=TRUE \
    _R_CHECK_TESTS_NLINES_=0 \
    _R_CHECK_TIMINGS_=0 \
    _R_CHECK_TOPLEVEL_FILES_=TRUE \
    _R_CHECK_USE_INSTALL_LOG_=FALSE \
    _R_CHECK_VC_DIRS_=TRUE \
    _R_CHECK_VIGNETTES_NLINES_=0 \
    _R_CHECK_LENGTH_1_CONDITION_="verbose,abort" \
    _R_CHECK_LENGTH_1_LOGIC2_="verbose,abort" \
    R_CHECK_CONSTANTS=5 \
    R_JIT_STRATEGY=3 \
    R CMD check "$(ls -t "$tmpd"/icd*.tar.gz | head -1)"
    popd

# N.b. R_CHECK_CONSTANTS and R_JIT_STRATEGY work together, but can make examples and tests run very slowly.
