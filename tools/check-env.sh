#!/usr/bin/env bash

# run a package check using a given set of environment variables

#shellcheck disable=SC2012,SC1091
#shellcheck source=find-icd-home.sh
set -eu
IFS=$'\n\t'
function whereami() {
    local dir SOURCE
    SOURCE="${BASH_SOURCE[0]}"
    while [ -h "$SOURCE" ]; do
        dir="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
        SOURCE="$(readlink "$SOURCE")"
        [[ $SOURCE != /* ]] && SOURCE="$dir/$SOURCE" 
    done
    cd -P "$( dirname "$SOURCE" )" 
    pwd
}

declare ICD_HOME
if [[ -z $ICD_HOME ]]; then 
    [[ -f "$(whereami)/find-icd-home.sh" ]] &&
        source "$(whereami)/find-icd-home.sh"

    if [[ $(type -t jack_icd_home) == function ]]; then {
        ICD_HOME="$(find_icd_home)"
    } else {
    echo "Searching for 'icd' directory in $HOME..." >&2
        ICD_HOME="$(shopt -s globstar; ls "$HOME/**/icd" | head -1)"
    } fi
fi

tmpd=$(mktemp -d --tmpdir icdcheckcranplus.XXXXXXXXXXX)
function finish {
    echo -n "Finished with $tmpd. Use:"
    echo -n "rm -rf $tmpd"
    echo "to delete it."
}
trap finish EXIT
cd "$tmpd"
# build with standard release options, i.e. compacting vignettes.
"${ICD_HOME}"/tools/build-full.sh
# for all environment variable options see here:
# https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Tools
set -x
export "$(xargs <"${ICD_HOME}"/tools/env/plus.env)"
printenv | grep CRAN
declare built_pkg
built_pkg="$(ls -t "$tmpd"/icd*.tar.gz | head -1)"
R CMD check "$built_pkg"
