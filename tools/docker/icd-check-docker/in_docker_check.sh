#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

R_PKG_TAR_GZ=$(ls -t ${R_PKG_NAME}*.tar.gz | tail -1)

# for all the flags
# https://cran.r-project.org/doc/manuals/r-release/R-ints.html
# not sure what happens if I us --as-cran and turn off a feature: seems like qpdf is still sought.

# consider using icd/tools/check_*.sh instead
ASAN_OPTIONS=abort_on_error=1 \
  _R_CHECK_DOC_SIZES_=false \
  _R_CHECK_CRAN_INCOMING_=false \
  _R_CHECK_FORCE_SUGGESTS_=false \
  $R_CMD CMD check $R_PKG_TAR_GZ

popd
