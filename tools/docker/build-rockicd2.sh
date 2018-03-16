#!/bin/bash

# Copyright (C) 2014 - 2018  Jack O. Wasey
#
# This file is part of icd.
#
# icd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd. If not, see <http:#www.gnu.org/licenses/>.

# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

set -x

pushd ${ICD_HOME:-$HOME/icd}/tools/docker/icd-check-docker

for DOCKER_BASE in r-clang-5.0 r-clang-trunk rocker/r-devel-ubsan-clang
do
DOCKER_ICD=${DOCKER_BASE}-icd
DOCKER_IMAGE=jackwasey/${DOCKER_BASE}

set -x

#https://docs.docker.com/engine/reference/run/#/env-environment-variables
           # -v "${TOOLS_DIR}/in_docker_check.sh":/go.sh \

docker build \
           --build-arg=ICD_PROJECT_NAME=${ICD_PROJECT_NAME:=icd} \
           --build-arg=R_PKG_NAME=${R_PKG_NAME:=$ICD_PROJECT_NAME} \
           --build-arg=GITHUB_URL=${GITHUB_URL:=https://github.com} \
           --build-arg=GITHUB_USER=${GITHUB_USER:=jackwasey} \
           --build-arg=GITHUB_REPO=${GITHUB_REPO:=$ICD_PROJECT_NAME} \
           --build-arg=GIT_BRANCH=${GITHUB_BRANCH:=master} \
           --build-arg=GIT_URL=${GIT_URL:=$GITHUB_URL/$GITHUB_USER/$GITHUB_REPO.git} \
           --build-arg=R_CMD=${R_CMD:=RD} \
	   --build-arg=DOCKER_IMAGE=$DOCKER_IMAGE \
	   -t ${DOCKER_ICD} \
           .
done

popd

