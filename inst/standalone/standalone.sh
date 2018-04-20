#!/bin/bash
set -x
# don't use this: use the Makefile. This is just for experimenting with paths

rcpp=$(Rscript -e 'cat(system.file(package = "Rcpp"))')
rinside=$(Rscript -e 'cat(system.file(package = "RInside"))')
r=$(R RHOME)
icd=$(Rscript -e 'cat(system.file(package = "icd"))')

clang++ \
	-I"${r}/include" \
	-I"${rcpp}/include" \
	-I"${rcpp}/include/Rcpp" \
	-I"${rinside}/include" \
	-I"${icd}/include" \
	-I"." \
	-g \
	-L"${rinside}/libs" \
	-L"${rinside}/lib" \
	-L"${icd}/libs" \
	-L"${r}/lib" \
	-lR \
	-o standalone.out \
	standalone.cpp
