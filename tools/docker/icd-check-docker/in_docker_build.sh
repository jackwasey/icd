#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# source /in_docker_get_icd.sh

# try also doing this within docker, as it doesn't work when container invoked
# without this constraint
old_ulimit=$(ulimit -s)
ulimit -s unlimited || true
# verify with Cstack_info()

function finish {
	ulimit -s "$old_ulimit" || true
}
trap finish EXIT

source /in_docker_ldpreload_asan.sh
ldpreload_asan

# --no-build-vignettes (without build, errors more visible at install step)
$R_CMD CMD build $GITHUB_REPO
R_PKG_TAR_GZ=$(ls -t ${R_PKG_NAME}*.tar.gz | tail -1)

popd
