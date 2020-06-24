#!/usr/bin/env bash
#set -euo pipefail
#set +o noglob
#shopt -s globstar
#shopt -s extglob
set -x

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
for bib in icd.bib icdjss.bib icdpkg.bib other.bib gplv3.bib
do {
	#[[ -f "${bib}" ]] || continue
	if grep -q '[^\]_' "${bib}" | grep -v ^@
	then
		[[ -n ${VERBOSE:-} ]] && echo "Underscores found in ${bib} entries. Fixing now." >&2
		bad=1
		# repeat many times, as I couldn't do this easily with the PERL one-liner
		sed -i '/[[:space:]]*url/s/\\?_/\\_/g' "${bib}"
	else
		[[ -n ${VERBOSE:-} ]] && echo "No underscores found in ${bib}" >&2
	fi
	# delete abstracts and other unwanted in article
	for del in abstract eprinttype
	do {
		perl -p -000 -i -e \
			's/[[:space:]]+'"${del}"'[[:space:]]+=[[:space:]]+\{[^}]*\},\n//g' \
			"${bib}"; }
		done
		bibtool -s -i "${bib}" -o "${bib}" -r bibtool.rsc
		#bibtool -s -i "${bib}" -o "${bib}" -- preserve.key.case=on
		#bibtool -s -i "${bib}" -o "${bib}" -- "delete.field = { abstract }"
		#bibtool -s -i "${bib}" -o "${bib}" -- "delete.field = { url }"
	}
done

exit "${bad}"
