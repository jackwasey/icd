#!/bin/bash

#set CXX="iwyu" then 
R CMD build --no-build-vignettes icd && MAKEFLAGS="-k" R CMD INSTALL icd_2.3.1.tar.gz 2>/tmp/icd-iwyu.out

