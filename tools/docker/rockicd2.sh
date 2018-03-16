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

#https://stackoverflow.com/questions/14719349/error-c-stack-usage-is-too-close-to-the-limit#14719448
# C stack limit problems hopefully fixed by this. Can also be set to "unlimited" Default is 8192k on my pc.
old_ulimit=$(ulimit -s)
ulimit -s unlimited

function finish {
	ulimit -s "$old_ulimit"
}
trap finish EXIT

# show what we're doing
# set -x

# check package using given (local) docker image. Won't work with straight rocker/r-base etc.
echo "Working directory: ${ICD_HOME:=$HOME/icd}"
DOCKER_IMAGE="${1:-r-clang-5.0}"

# ROCK_TMP=`mktemp -d 2>/dev/null || mktemp -d -t 'mytmpdir'`

if [[ ! $DOCKER_IMAGE =~ (jackwasey\/)r-.+ ]]; then
   echo "Not using R from a jackwasey docker image"
fi

if [[ ! $DOCKER_IMAGE =~ ^rocker\/ && ! $DOCKER_IMAGE =~ ^rhub\/ && ! $DOCKER_IMAGE =~ ^jackwasey\/ ]]; then
  DOCKER_IMAGE="jackwasey/${DOCKER_IMAGE}"
fi

# drop trailing slash
DOCKER_IMAGE=${DOCKER_IMAGE/%\//}
echo "using docker image: $DOCKER_IMAGE"

TOOLS_DIR="$ICD_HOME/tools/docker"

echo "Environment:"
echo "ICD_PROJECT=${ICD_PROJECT_NAME:=icd}"
echo "R_PKG_NAME=${R_PKG_NAME:=$ICD_PROJECT_NAME}"
echo "GITHUB_URL=${GITHUB_URL:=https://github.com}"
echo "GITHUB_USER=${GITHUB_USER:=jackwasey}"
echo "GITHUB_REPO=${GITHUB_REPO:=$ICD_PROJECT_NAME}"
echo "GIT_BRANCH=${GIT_BRANCH:=master}"
echo "GIT_URL=${GIT_URL:=$GITHUB_URL/$GITHUB_USER/$GITHUB_REPO.git}"
echo "R_CMD=${R_CMD:=RD}"

# expands variables and prints command
set -x

#https://docs.docker.com/engine/reference/run/#/env-environment-variables
           # -v "${TOOLS_DIR}/in_docker_check.sh":/go.sh \
docker run \
           -e "ICD_PROJECT_NAME=$ICD_PROJECT_NAME" \
           -e "R_PKG_NAME=$R_PKG_NAME" \
           -e "GITHUB_URL=$GITHUB_URL" \
           -e "GITHUB_USER=$GITHUB_USER" \
           -e "GITHUB_REPO=$GITHUB_REPO" \
           -e "GIT_BRANCH=$GIT_BRANCH" \
           -e "GIT_URL=$GIT_URL" \
           -e "R_CMD=$R_CMD" \
           --rm \
	   -ti \
	   --cap-add SYS_PTRACE \
	   "$DOCKER_IMAGE" \
	   ${2:-/in_docker_base.sh}

