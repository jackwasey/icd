#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# loop through all of my rocker images with specially built R. Don't include the
# clang builds which happen to have R installed.
for d in $(docker images | awk '/jackwasey\/r-/{print $1}'); do
  echo "rocking: ${d}"
  ./rock-icd "${d}"
done
