#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

source in_docker_get_icd.sh

# try also doing this within docker, as it doesn't work when container invoked without this constraint
old_ulimit=$(ulimit -s)
ulimit -s unlimited
# verify with Cstack_info()

function finish {
	ulimit -s "$old_ulimit"
}
trap finish EXIT

source in_docker_ldpreload_asan.sh

# actually, we need to build based on the directory name, not the package name:
$R_CMD CMD build $GITHUB_REPO # --no-build-vignettes (without build, errors more visible at install step)
R_PKG_TAR_GZ=$(ls -t ${R_PKG_NAME}*.tar.gz | tail -1)

# for all the flags
# https://cran.r-project.org/doc/manuals/r-release/R-ints.html
# not sure what happens if I us --as-cran and turn off a feature: seems like qpdf is still sought.
ASAN_OPTIONS=abort_on_error=1 \
_R_CHECK_DOC_SIZES_=false \
_R_CHECK_CRAN_INCOMING_=false \
_R_CHECK_FORCE_SUGGESTS_=false \
$R_CMD CMD check $R_PKG_TAR_GZ
popd
