#!/bin/bash
set -euo pipefail
IFS=$'\n\t'
${ICD_HOME:-$HOME/icd}/tools/build.sh
R CMD INSTALL --no-clean-on-error \
	--debug \
       	--install-tests \
	"$(ls -t icd_*.tar.gz | head -1)"
# ${ICD_HOME:-$HOME/icd}

