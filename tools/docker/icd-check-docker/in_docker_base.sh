#!/bin/bash
set -euo pipefail
IFS=$'\n\t'
set -x
#https://stackoverflow.com/questions/14719349/error-c-stack-usage-is-too-close-to-the-limit#14719448
# C stack limit problems hopefully fixed by this.
# Can also be set to "unlimited" Default is 8192k on my pc.
# do this inside and outside the docker container
old_ulimit=$(ulimit -s)
ulimit -s unlimited
function finish {
        ulimit -s "$old_ulimit"
}
trap finish EXIT
pushd /tmp

export MAKEFLAGS=-j8

if [ -f /etc/os-release ]; then
    source /etc/os-release
    OS=$NAME
elif type lsb_release >/dev/null 2>&1; then
    OS=$(lsb_release -si)
elif [ -f /etc/lsb-release ]; then
    source /etc/lsb-release
    OS=$DISTRIB_ID
elif [ -f /etc/debian_version ]; then
    OS=Debian
else
    OS=$(uname -s)
fi
# do all fedora, debian and ubuntu stuff, and blunder through errors
#if [[ "$OS" =~ ".*Ubuntu.*" || "$OS" =~ ".*Debian.*" ]]; then
echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections
apt-get update -qq || true
# apt-get dist-upgrade -qq -y || true
# libssh2-1-dev (optional for git but has debian version
# problem at least in April 2017)
for aptpkg in \
  git \
  libssl-dev \
  libxml2-dev \
  curl \
  libcurl4-openssl-dev \
  unixodbc-dev \
  qpdf \
  pandoc \
  pandoc-citeproc \
; do
  echo "checking $aptpkg"
  #PKG_OK=$(dpkg-query -W --showformat='${Status}\n' "$aptpkg" | grep "install ok installed")
  #if [[ -z "$PKG_OK" ]]; then
    echo "install $aptpkg"
    DEBIAN_FRONTEND=noninteractive apt-get -q -y install "$aptpkg"
  #else
  #  echo "$aptpkg already installed"
  #fi
done

#elif [[ "$OS" =~ ".*Fedora.*" ]]; then
# dnf install -y git libxml2-devel || true
#fi
echo "R CMD: using '${R_CMD:-RD}'"

# tolerate R_CMD unset or empty, and default to RD if empty or unset:
# TODO: actually, RD is not available in all docker images, e.g. most basic
# rocker/tidyverse, verse, etc.
echo "checking ${R_CMD} exists"
if ! command -v "${R_CMD:-RD}" &>/dev/null; then
  echo "setting R_CMD to R"
  R_CMD=R
fi

source /in_docker_ldpreload_asan.sh
ldpreload_asan

# these are always checked for, so we don't care which R is installed.
# We also need to re-install some packages, for some reason unclear to me:
# https://github.com/rocker-org/r-devel-san-clang/issues/12
for pkg in \
  testthat \
  RODBC \
  xml2 \
  Rcpp \
  stringi \
  knitr \
  rmarkdown \
  microbenchmark \
; do
ASAN_OPTIONS=abort_on_error=0,detect_leaks=0 \
  ${R_CMD}script -e "if (any(grepl(\"${pkg}\", rownames(installed.packages())))) install.packages(\"${pkg}\")"
done

# We need to build based on the directory name, not the package name. Don't
# build vignettes now: they get built during check anyway.
#
# $R_CMD CMD build --no-build-vignettes $GITHUB_REPO
popd # out of /tmp
