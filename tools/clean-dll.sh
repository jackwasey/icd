#!/usr/bin/env bash

# find the package root from where we are, and clean the compilation artifacts
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

Rscript -e 'pkgbuild::clean_dll()'
