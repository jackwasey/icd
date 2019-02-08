#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

/in_docker_build.sh
/in_docker_check.sh

