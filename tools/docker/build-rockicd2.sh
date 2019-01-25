#!/bin/bash
set -euo pipefail
IFS=$'\n\t'
set -x

pushd ${ICD_HOME:-$HOME/icd}/tools/docker/icd-check-docker

for DOCKER_IMAGE in r-clang-5.0 r-clang-trunk rocker/r-devel-ubsan-clang rocker/r-devel-san rocker/r-devel
do
if [[ ! $DOCKER_IMAGE =~ (jackwasey\/)r-.+ ]]; then
   echo "Not using R from a jackwasey docker image"
fi
if [[ ! $DOCKER_IMAGE =~ ^rocker\/ && ! $DOCKER_IMAGE =~ ^rhub\/ && ! $DOCKER_IMAGE =~ ^jackwasey\/ ]]; then
  DOCKER_IMAGE="jackwasey/${DOCKER_IMAGE}"
fi
# strip the rocker/ or other prefix
DOCKER_ICD=$(sed 's/^[A-Za-z]*\///' <(echo $DOCKER_IMAGE))-icd

#https://docs.docker.com/engine/reference/run/#/env-environment-variables
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
	   -t jackwasey/${DOCKER_ICD} \
           .
done
popd
