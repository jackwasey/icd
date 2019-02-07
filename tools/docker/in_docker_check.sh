#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# call from base: source /in_docker_ldpreload_asan.sh
source /in_docker_base.sh
source /in_docker_get_icd.sh
source /in_docker_build_check.sh
