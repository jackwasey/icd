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
#RCPPLIBS := 		$(shell echo 'Rcpp:::LdFlags()'  | $(R_HOME)/bin/R --vanilla --slave)


## include headers and libraries for RInside embedding classes
RINSIDEINCL := 		$(shell echo 'RInside:::CxxFlags()' | $(R_HOME)/bin/R --vanilla --slave)
RINSIDELIBS := 		$(shell echo 'RInside:::LdFlags()'  | $(R_HOME)/bin/R --vanilla --slave)

## compiler etc settings used in default make rules
CXX := 			$(shell $(R_HOME)/bin/R CMD config CXX11)
CPPFLAGS := 		-Wall $(shell $(R_HOME)/bin/R CMD config CPPFLAGS)
CXXFLAGS := 		$(RCPPFLAGS) $(RCPPINCL) $(RINSIDEINCL) $(shell $(R_HOME)/bin/R CMD config CXX11FLAGS) -DICD9_STANDALONE
LDLIBS := 		$(RLDFLAGS) $(RRPATH) $(RBLAS) $(RLAPACK) $(RINSIDELIBS) #$(RCPPLIBS)

all: 			$(programs)
##			@test -x /usr/bin/strip && strip $^

standalone: standalone.cpp
## build and link in one step
##			$(CXX) $(LDFLAGS) $(CXXFLAGS) -o standalone.bin standalone.cpp $(LDLIBS) -DICD9_STANDALONE


run:			$(programs)
			@for p in $(programs); do echo; echo "Running $$p:"; ./$$p; done

clean:
			rm -vf $(programs)
			rm -vrf *.dSYM

runAll:
			for p in $(programs); do echo ""; echo ""; echo "Running $$p"; ./$$p; done
