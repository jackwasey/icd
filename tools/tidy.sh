#!/usr/bin/env bash

set -euo pipefail

## call with --fix to have clang-tidy actually modify code
## see src/.clang-tidy

declare R_inc_path Rcpp_inc_path RcppEigen_inc_path ct

r_pkg_inc_path() {
  local rcmd ipath rpath
  rcmd="$(printf 'cat(base::system.file(package = "%s", "include"))' "${1?}")"
  ipath="$(Rscript --default-packages=NULL -e "${rcmd}")"
  ## use GNU readlink from Homebrew on macOS, if available, for -f
  [ -r "${ipath}" ] || {
    echo "${ipath} found for '$1' but not readable" >&2
    return 1
  }
  rl=greadlink
  command -v "${rl}" >/dev/null || rl=readlink
  rpath="$("${rl}" -f "${ipath}")"
  [ -r "${rpath}" ] || {
    echo "${rpath} resolved for '$ipath' but not readable" >&2
    return 2
  }
  printf "%s" "${rpath}"
}

runct() {
  local -a ct_args clang_args
  local R_inc_path Rcpp_inc_path RcppEigen_inc_path

  R_inc_path="$(Rscript --silent -e 'cat(Sys.getenv("R_INCLUDE_DIR"))')"
  Rcpp_inc_path="$(r_pkg_inc_path Rcpp)"
  RcppEigen_inc_path="$(r_pkg_inc_path RcppEigen)"
  ct_args=(
    ## hide info on suppressed warnings from system headers:
    --quiet
    ## use .clang.format when '--fix'ing
    --format-style=file
    ## This ultimately stops package code warnings being surfaced:
    #--line-filter='[{"name":"RcppExports.cpp"}]'
  )
  clang_args=(
    -isystem "${R_inc_path}"
    -isystem "${Rcpp_inc_path}"
    -isystem "${RcppEigen_inc_path}"
    #    --system-header-prefix=R_ext/
    #    --system-header-prefix=Rcpp/
    #    --system-header-prefix=RcppEigen/
    #    --system-header-prefix=unsupported/
    #    --system-header-prefix=internal/
    #    --system-header-prefix=Eigen/
    #    --system-header-prefix=Eigen/src/
    #    --system-header-prefix=Eigen/src/SparseCore/
    #    --system-header-prefix=SparseCore/
    #    --system-header-prefix=src/SparseCore/
    #    --system-header-prefix=Rcpp/internal/
    -DICD_CLANG_TIDY
    -DNDEBUG
    -fpic -pipe -w -std=c++17
  )
  while [ "x${1-}" != "x--" ]; do
    ct_args+=(${1-})
    shift || break
  done
  shift || true
  while [ $# -gt 0 ]; do
    clang_args+=("${1?}")
    shift || break
  done

  echo "Running clang-tidy:" >&2
  echo "${ct}" "${ct_args[@]}" -- "${clang_args[@]}" >&2
  "${ct}" "${ct_args[@]}" -- "${clang_args[@]}"
}

while ! [ -r DESCRIPTION ]; do
  cd .. 2>/dev/null || break
done
if ! grep -q -e "^Package: icd$" DESCRIPTION; then
  echo "${PWD} does not contain icd: '$(head -1 DESCRIPTION)'" >&2
  exit 1
fi
cd src || {
  echo "cannot enter src/" >&2
  exit 1
}

for _n in -{15..3} ''; do
  ct="clang-tidy${_n}"
  command -v "${ct}" >/dev/null && break
done

echo "Using clang-tidy: ${ct}..." >&2

echo "running ${ct} with arguments: $*" >&2
if [[ $* =~ .*\.[ch]pp ]]; then
  runct "$@"
else
  runct "$@" ./*.hpp ./*.cpp
fi
