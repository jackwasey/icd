#!/usr/bin/env bash

usage() {
cat >&2 <<"_EOF"
Clean-up bibtex export from Zotero.

Since JSS uses natbib, 'Better BibTex' is correct, not 'Better BibLaTex'. The
Zotero export citation style format is:
[auth:capitalize:postfix=_][shorttitle3_3:postfix=_][year]

Using options "Force citation key to plain text," "Export unicode as plain-text latex
commands," omitting "file,pmcid,abstract,pmid" although these are later deleted
by bibtools. Also checked "Include comments about potential problems with the
references." bibtool will remove these comments later.

Once ready to resubmit, pin the JSS citation keys. Check that there are no
unused references in the JSS submission.

_EOF
}
trap usage EXIT

set -euo pipefail

command -v bibtool >/dev/null 2>&1 || {
    echo >&2 "I require bibtool but it's not installed. Aborting."
    exit 1
}

if [ -d vignettes ]; then
    cd vignettes || { echo "cannot change to vignette directory"; exit 1; }
else
    echo "No vignettes directory found. Assuming in it."
fi

declare -i bad=0

# make sure there are no unescaped underscores in the URLs:
for bib in *.bib
do {
    [[ -f "${bib}" ]] || { echo "${bib} not found. Continuing..."; continue; }
    echo "Working on ${bib}"
    grep -q '__' "${bib}" && echo "ERROR: double underscore in ${bib}" >&2 && exit 1
    if grep -q '[^\]_' "${bib}" | grep -v ^@
    then
        [[ -n ${VERBOSE:-} ]] && echo "Underscores found in ${bib} entries. Fix?" >&2
        bad=1
        # may need to repeat many times, as I couldn't do this easily with a one-liner
        echo sed -i '/[[:space:]]*url/s/\\?_/\\_/g' "${bib}"
    else
        [[ -n ${VERBOSE:-} ]] && echo "No underscores found in ${bib}" >&2
    fi
    # clean up bibtex output frmo zotero. JSS, uses natbib, which is BibTex-based, not BibLaTex
    bibtool -s -i "${bib}" -o "${bib}" -r bibtool.rsc
}
done

exit "${bad}"
