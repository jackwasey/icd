#!/bin/sh

#--use-c++=/usr/local/bin/clang++ \
#--use-cc=/usr/local/bin/clang \

scan-build -analyze-headers \
clang++ -I/usr/share/R/include -DNDEBUG -I. \
-I"/usr/local/lib/R/site-library/Rcpp/include"  -fopenmp -fpic \
-O0 -fstack-protector-strong -Wall \
-D_FORTIFY_SOURCE=2 -g  -c *.cpp *.h

