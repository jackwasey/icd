#!/bin/sh

# Copyright (C) 2014 - 2015  Jack O. Wasey
#
# This file is part of icd9.
#
# icd9 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd9 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd9. If not, see <http:#www.gnu.org/licenses/>.


# check package using the standard GCC santizer build from rocker,

ICD9_HOME=$HOME/Documents/RProjects/icd9
cd $ICD9_HOME/..
R CMD build --no-build-vignettes icd9
ICD9_LATEST_PKG=`ls -tr | grep icd9.*gz$ | tail -1`
#ICD9_DOCKER_IMAGE=rocker/r-devel-ubsan-clang
# use my updated version:
ICD9_DOCKER_IMAGE=r-devel-ubsan-clang
ICD9_TMP=/tmp/$ICD9_DOCKER_IMAGE
rm -rf $ICD9_TMP
mkdir -p $ICD9_TMP
cp $LATEST_ICD9 $ICD9_TMP
echo "warning - ensure that 'check.r' actually invokes the newly compiled R-devel version with the sanitizer!"

docker build -t $ICD9_DOCKER_IMAGE $ICD9_HOME/tools/$ICD9_DOCKER_IMAGE

# pre-install binaries for deb packages doesn't work because check.r installs dependencies from source regardless.
# use unstable to get the right development library versions
# the output files are available outside the docker image in the working directory.
# docker run --rm -ti -u="jack" -v $ICD9_TMP:/mnt $ICD9_DOCKER_IMAGE sh -c "apt-get update && apt-get --yes dist-upgrade && apt-get --yes install libxml2-dev/unstable libssl-dev/unstable libssh2-1-dev/unstable littler && /usr/share/doc/littler/examples/check.r --setwd /mnt --install-deps $ICD9_LATEST_PKG || cat /mnt/icd9.Rcheck/00install.out"

docker run --rm -ti -u=1000 -v $ICD9_TMP:/mnt $ICD9_DOCKER_IMAGE

