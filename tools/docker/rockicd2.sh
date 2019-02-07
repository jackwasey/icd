#!/bin/bash

set -euo pipefail
IFS=$'\n\t'
set -x
#https://stackoverflow.com/questions/14719349/error-c-stack-usage-is-too-close-to-the-limit#14719448
# C stack limit problems hopefully fixed by this. Can also be set to "unlimited" Default is 8192k on my pc.
old_ulimit=$(ulimit -s)
ulimit -s unlimited
# verify with Cstack_info()
function finish {
	ulimit -s "$old_ulimit"
}
trap finish EXIT
echo "Working directory: ${ICD_HOME:=$HOME/icd}"
DOCKER_IMAGE="${1:-r-clang-trunk}"

if [[ ! "$DOCKER_IMAGE" =~ "(jackwasey/)r-.+" ]]; then
   echo "Not using R from a jackwasey docker image"
fi

if [[ ! "$DOCKER_IMAGE" =~ "^rocker/" && ! "$DOCKER_IMAGE" =~ "^rhub/" && ! "$DOCKER_IMAGE" =~ "^jackwasey/" ]]; then
  DOCKER_IMAGE="jackwasey/${DOCKER_IMAGE}"
fi
# drop trailing slash
DOCKER_IMAGE=${DOCKER_IMAGE/%\//}
echo "using docker image: $DOCKER_IMAGE"
TOOLS_DIR="$ICD_HOME/tools/docker"
set -x
#https://docs.docker.com/engine/reference/run/#/env-environment-variables
docker run \
	-v "${TOOLS_DIR}/in_docker_check.sh":/in_docker_check.sh \
	-v "${TOOLS_DIR}/in_docker_base.sh":/in_docker_base.sh \
	-v "${TOOLS_DIR}/in_docker_get_icd.sh":/in_docker_get_icd.sh \
	-v "${TOOLS_DIR}/in_docker_build_check.sh":/in_docker_build_check.sh \
	-v "${TOOLS_DIR}/in_docker_ldpreload_asan.sh":/in_docker_ldpreload_asan.sh \
	-e "ICD_PROJECT_NAME=${ICD_PROJECT_NAME:=icd}" \
	-e "R_PKG_NAME=${R_PKG_NAME:=$ICD_PROJECT_NAME}" \
	-e "GITHUB_URL=${GITHUB_URL:=https://github.com}" \
	-e "GITHUB_USER=${GITHUB_USER:=jackwasey}" \
	-e "GITHUB_REPO=${GITHUB_REPO:=$ICD_PROJECT_NAME}" \
	-e "GIT_BRANCH=${GIT_BRANCH:=master}" \
	-e "GIT_URL=${GIT_URL:=$GITHUB_URL/$GITHUB_USER/$GITHUB_REPO.git}" \
	-e "R_CMD=${R_CMD:=RD}" \
	--rm -ti \
  --cap-add SYS_PTRACE \
  "$DOCKER_IMAGE" \
  "${2:-/in_docker_build_check.sh}"
