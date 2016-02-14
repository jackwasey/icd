#!/bin/sh

# Copyright (C) 2014 - 2016  Jack O. Wasey
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


# check package using given (local) docker image. Won't work with straight rocker/r-base etc.
: ${ICD9_HOME:=$HOME/Documents/RProjects/icd9}
: ${ICD9_DOCKER_IMAGE:=$1}
: ${ICD9_DOCKER_SCRIPT:=build_install_check_in_docker.sh}

# strip directory symbol to get the docker image name from a dir with trailing slash
IM_NM=`echo $ICD9_DOCKER_IMAGE | tr -d "/"`

IM=$ICD9_HOME/tools/$IM_NM
SC=$ICD9_HOME/tools/$ICD9_DOCKER_SCRIPT

if [ ! -d "$IM" ]; then
  echo "Docker image should be a directory under tools in the icd9 project."
  exit 1
fi

cp $SC $IM
docker build -t $IM_NM $IM
rm -f $IM/$ICD9_DOCKER_SCRIPT
docker run --rm -ti $IM_NM


