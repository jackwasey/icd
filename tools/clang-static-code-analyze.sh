#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

#--use-c++=/usr/local/bin/clang++ \
#--use-cc=/usr/local/bin/clang \

# alternative is to set CXX and CC to scan-build

scan-build -analyze-headers \
	clang++ -g \
		-I/usr/share/R/include \
		-DNDEBUG \
		-I. \
		-I"/usr/local/lib/R/site-library/Rcpp/include" \
		-fpic \
		-O0 \
		-fstack-protector-strong -Wall \
		-D_FORTIFY_SOURCE=2
		-c *.cpp *.h *.c

