#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

# now do something like:
# MAKEFLAGS="-k CCACHE= CXX11=/usr/bin/iwyu" tools/build-binary-quick.sh --preclean

declare SED=sed
command -v gsed >/dev/null && SED=gsed

declare IH
#old_pwd="${PWD}"
if [[ -z ${IH-} ]]; then
	for d in "${PWD}" "${PWD}"/.. "${PWD}"/../..; do
		[[ -e "${d}/DESCRIPTION" ]] &&
			IH="${d}" &&
			break

		done
fi
cd "${IH?}" || { echo "cannot cd to ${IH?}" >&2; exit 1; }
[[ -e "DESCRIPTION" ]] || {
	echo "still cannot find R package source root" >&2
	exit 1
}

Rscript -e 'Rcpp::compileAttributes()'
# insert pragmas for persistent g++ warning
"$SED" -i -e '/BEGIN_RCPP/i#pragma GCC diagnostic push\n#pragma GCC diagnostic ignored "-Wcast-function-type"' -e "/END_RCPP/a#pragma GCC diagnostic pop" src/RcppExports.cpp || exit 1
"$SED" -i -e '/R_CallMethodDef/i#pragma GCC diagnostic push\n#pragma GCC diagnostic ignored "-Wcast-function-type"' -e "/R_init_icd/i#pragma GCC diagnostic pop" src/RcppExports.cpp || exit 1

tmp_lib="$(mktemp -d)"
cd "${tmp_lib}" || { echo "cannot cd to ${tmp_lib}."; exit 1;}

# quickly build the pacakge, and put it in the current directory
R_MAKEVARS_USER=${IH?}/tools/mk/Makevars.quick \
R CMD INSTALL \
    -d \
    --build \
    --library="${tmp_lib}" \
    --libs-only \
    --no-docs \
    --no-data \
    --no-test-load \
    --no-clean-on-error \
    "$@" \
    "${IH}"

echo "Output in ${tmp_lib}"
