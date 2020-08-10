#!/usr/bin/env bash
set -euo pipefail
echo -n "Running clang-format using path: "
command -v clang-format || { echo "clang-format not found" >&2; exit 1; }
echo -n "clang-format version: "
clang-format --version
echo "${COLOR_GREEN:-}Also note R/devel.R has 'f' to do this and styler${COLOR_NC:-}"
find "${1:-.}" \
	-type d \( -name "benchmarks" \) -prune \
	-o -type f -a \( \
	-name '*.c' \
	-o -name '*.h' \
	-o \( -name '*.cpp' -a -not -name 'RcppExports.cpp' \) \
	\) -print |
	while read -r f; do
		echo "${f}"
		clang-format -i "$f";
	done

# Rscript -e 'styler::style_pkg(filetype = c("R", "Rprofile", "Rmd", "Rnw"))'
