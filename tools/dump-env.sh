#!/usr/bin/env bash
set +e
#set -x
# dump all possibly relevant environment, to help Travis debugging in particular.
#exec >&2

# System
uname -a
pwd
printenv | sort
## macos, homebrew
if command -v brew >/dev/null; then
  brew list
  brew doctor
  brew environment
fi
## debian/ubuntu
if command -v dpkg >/dev/null; then
  dpkg -l | grep -e r-cran -e r-base
  dpkg -L r-cran-rcpp | sed 's/Rcpp\/.*$/Rcpp/' | sort -u || true
  dpkg -L r-cran-rcppeigen | sed 's/RcppEigen\/.*$/RcppEigen/' | sort -u || true
fi

command -v R
command -v Rscript

RH="$(R RHOME)"
RHE="${RH}/etc"

echo "R RHOME gives '${RH}'" >&2

#find "${HOME}/.R" -print -exec cat '{}' \;
for f in {"${RHE}","${HOME}"}/{,./,.R/}{,check-,build-}{ldpaths,Makeconf,Makevars,Renviron,Rprofile}{.site,}; do
  if [[ -f $f ]]; then
    echo "FOUND: $f"
    cat "$f"
    echo
  fi
done

R CMD config --all

ls -R ~/.R

# R

## libraries
declare -a pths
pths=("${HOME}/R/Library"
  "${HOME}"/R-bin/lib/R/library
  "${RH}"/{site-,}library
  /usr/{local/,}lib/R/{site-,}library
)
for lib in "${pths[@]}"; do
  echo "R library path: $lib is present"
  ls -ld "$lib"
done

R --no-save <<_EOF
options()
options(echo=TRUE)
R.version
print.default(.libPaths(), quote = FALSE)
.Library
.Library.site
#.Machine
.Platform
Sys.info()
.dynLibs()
searchpaths()
search()
capabilities()
sessionInfo()
extSoftVersion()
if (requireNamespace("sessioninfo", quietly = TRUE, warn.conflicts = FALSE)) sessioninfo::session_info()
_EOF

# this package
[[ -f .Rbuildignore ]] && cat .Rbuildignore || echo "No .Rbuildignore!" >&2
[[ -f .Rinstignore ]] && cat .Rinstignore || echo "No .Rinstignore" >&2
cat DESCRIPTION || echo "No DESCRIPTION!" >&2

true
