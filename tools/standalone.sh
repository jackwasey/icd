#!/bin/sh
clang++ \
	-I"/usr/share/R/include" \
	-I"/home/jack/R/x86_64-pc-linux-gnu-library/3.2/Rcpp/include/" \
	-I"/home/jack/R/x86_64-pc-linux-gnu-library/3.2/Rcpp/include/Rcpp/" \
	-I"/home/jack/R/x86_64-pc-linux-gnu-library/3.2/RInside/include/" \
	-I. \
	-std=c++11 -g -Wall -v \
	-L"/usr/lib/R/lib/" \
	-L"/home/jack/R/x86_64-pc-linux-gnu-library/3.2/Rcpp/libs/" \
	-lR \
	-DICD9_STANDALONE \
	-o standalone.bin \
	standalone.cpp 

	#-lRcpp  -c \
