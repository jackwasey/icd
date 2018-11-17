#!/bin/bash
tmpd=$(mktemp -d /tmp/icdvalgrind.XXXXXXXXXXX)
function finish {
#	  rm -rf "$tmpd"
  echo "Not cleaning $tmpd"
}
trap finish EXIT
#rsync -r --exclude=".git" "${ICD_HOME:-$HOME/rprojects/icd}" "$tmpd"
pushd "$tmpd"

${ICD_HOME:-$HOME/rprojects/icd}/tools/build.sh --no-build-vignettes --no-manual --resave-data=no

# --use-valgrind needs a .valgrindrc or VALGRIND_OPTS for options. If just running R with valgrind,
# this is not necessary, can just specify options in the R -d "valgrind ..." command.


# callgrind options:
# export VALGRIND_OPTS="--separate-threads=yes --simulate-cache=yes"

# mem
#--leak-check=full
#--track-origins=yes
#--show-leak-kinds=all

#export VALGRIND_OPTS="--leak-check=full --track-origins=yes --show-leak-kinds=all --callgrind:instr-atstart=no"
R CMD check \
  --no-build-vignettes \
  --use-valgrind \
  "$(ls -t $tmpd/icd*.tar.gz | head -1)"
popd
