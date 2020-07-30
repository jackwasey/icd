#!/usr/bin/env bash
echo -n "Running clang-format using path: "
command -v clang-format || { echo "clang-format not found" >&2; exit 1; }
echo -n "clang-format version: "
clang-format --version

find "${1:-.}" \
	-name '*.c' \
	-o -name '*.h' \
	-o \( -name '*.cpp' -a -not -name 'RcppExports.cpp' \) |
	while read -r f; do
		xargs clang-format -i "$f";
	done
