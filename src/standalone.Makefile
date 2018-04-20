## -*- mode: make; tab-width: 8; -*-
##
## Simple Makefile
##

## comment this out if you need a different version of R,
## and set set R_HOME accordingly as an environment variable
R_HOME := 		$(shell R RHOME)

sources := 		$(wildcard *.cpp)
programs := 		$(sources:.cpp=)

## include headers and libraries for R
RCPPFLAGS := 		$(shell $(R_HOME)/bin/R CMD config --cppflags)
RLDFLAGS := 		$(shell $(R_HOME)/bin/R CMD config --ldflags)
RBLAS := 		$(shell $(R_HOME)/bin/R CMD config BLAS_LIBS)
RLAPACK := 		$(shell $(R_HOME)/bin/R CMD config LAPACK_LIBS)

## if you need to set an rpath to R itself, also uncomment
#RRPATH :=		-Wl,-rpath,$(R_HOME)/lib

## include headers and libraries for Rcpp interface classes
## note that RCPPLIBS will be empty with Rcpp (>= 0.11.0) and can be omitted
RCPPINCL := 		$(shell echo 'Rcpp:::CxxFlags()' | $(R_HOME)/bin/R --vanilla --slave)

## include headers and libraries for RInside embedding classes
RINSIDEINCL := 		$(shell echo 'RInside:::CxxFlags()' | $(R_HOME)/bin/R --vanilla --slave)
RCPPEIGENINCL := 		$(shell echo 'RcppEigen:::CxxFlags()' | $(R_HOME)/bin/R --vanilla --slave)
RINSIDELIBS := 		$(shell echo 'RInside:::LdFlags()'  | $(R_HOME)/bin/R --vanilla --slave)
ICD := 		$(shell echo 'cat(system.file(package="icd"))' | $(R_HOME)/bin/R --vanilla --slave)
ICDINCL :=    -I$(ICD)/include -I$(HOME)/icd/src
ICDLIBS := 		-L$(ICD)/libs

## compiler etc settings used in default make rules
CXX := 			$(shell $(R_HOME)/bin/R CMD config CXX11)
CPPFLAGS := 		-Wall $(shell $(R_HOME)/bin/R CMD config CPPFLAGS)
CXXFLAGS := 		 -DICD_STANDALONE $(ICDINCL) $(RCPPEIGENINCL) $(RCPPFLAGS) $(RCPPINCL) $(RINSIDEINCL) $(shell $(R_HOME)/bin/R CMD config CXX11FLAGS) -fno-openmp
LDLIBS := 		$(ICDLIBS) $(RLDFLAGS) $(RRPATH) $(RBLAS) $(RLAPACK) $(RINSIDELIBS) -fno-openmp

all: 			$(programs)
##		@test -x /usr/bin/strip && strip $^

## all:          standalone
standalone: standalone.o ; $(CXX) $(LDFLAGS) -o standalone standalone.o $(LDLIBS)
standalone.o: standalone.cpp ; $(CXX) $(CXXFLAGS) -c standalone.cpp

