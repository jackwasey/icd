#!/usr/bin/env bash

#all=all.bib

command -v bibtool >/dev/null 2>&1 || { echo >&2 "I require bibtool but it's not installed. Aborting."; exit 1; }

if [ -d vignettes ]; then
  pushd vignettes || { echo "cannot change to vignette directory"; exit 1; }
  trap popd EXIT
fi

# make sure there are no unescaped underscores in the URLs:
for bib in icdjss.bib icdpkg.bib gplv3.bib other.bib
do
  if grep '[^\]_' "$bib" | grep -v ^@
  then
    echo "Underscores found in $bib entries. Fixing now."
    for n in {0..12}
    do
        [[ -v VERBOSE ]] && "echo ${n}"
      echo -n "."
      perl -pi -e 's/(url.*[^\\])_(.*)/\1\\_\2/g' "$bib"
    done
  else
    echo -e "\nNo underscores found in $bib" >&2
  fi
done

#bibtool -s gplv3.bib other.bib icdjss.bib -- preserve.key.case=on > "$all"
exit 0
