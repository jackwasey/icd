#!/usr/bin/env bash

# find icd source/development directory, warn if not where expected

# this should be sourced, not executed

find_icd_home() {
    [[ -n ${ICD_HOME:-} ]] && echo "$ICD_HOME" && return 0
    declare -a try_dirs
    try_dirs=(
    "${HOME}/icd"
    "${HOME}/rprojects/icd"
    "${HOME}/work/icd"
    )
    for d in "${try_dirs[@]}"; do
        [[ -e "$d" ]] &&
            [[ -d "$d" ]] &&
            [[ -f "${d}/DESCRIPTION" ]] &&
            echo "${d}" &&
            return 0

        continue
    done
    echo "Unable to find icd development directory in usual places. Finding first from $HOME" >&2
    find "$HOME" -maxdepth 3 -name "icd" -type d | head -1
}

(return 0 2>/dev/null) || {
    echo "This script should be sourced. Then call bash function 'find_icd_home'" >&2; exit 1; }
