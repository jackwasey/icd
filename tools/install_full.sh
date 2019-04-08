#!/bin/bash
set -euo pipefail
IFS=$'\n\t'
${ICD_HOME:-$HOME/icd}/tools/build.sh
MAKEFLAGS=-j$(getconf _NPROCESSORS_ONLN) \
	R CMD INSTALL \
		--no-clean-on-error \
		--debug \
		--install-tests \
		--data-compress=none \
		--no-resave_data \
		"$(ls -t icd_*.tar.gz | head -1)"
