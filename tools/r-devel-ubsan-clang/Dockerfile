## Emacs, make this -*- mode: sh; -*-

## start with the Docker 'base R' Debian-based image
FROM rocker/r-devel-ubsan-clang

# FROM HERE IS ICD9 specific

## I think that, although this seems like a good idea, once R-dev is installed, it would need many or all of these to be rebuilt, since the debian ones are just built against some debian archive version of R.
## RUN apt-get update -y && apt-get install -y -t unstable libxml2-dev libssl-dev r-base-core r-cran-xml r-cran-ggplot2 r-cran-rcurl r-cran-bitops r-cran-brew r-cran-rcolorbrewer r-cran-rcpp r-cran-dichromat r-cran-munsell r-cran-checkmate r-cran-evaluate r-cran-plyr r-cran-gtable r-cran-reshape2 r-cran-scales r-cran-proto r-cran-catools r-cran-testthat r-cran-memoise r-cran-digest r-cran-xtable

## do install these pesky libraries which must come from unstable (sid), and not the testing repo which is the default.
RUN apt-get update -y && apt-get install -y -t unstable libxml2-dev libssl-dev 

# don't rename R development version to RD, we want 'R' to invoke the development version!
## RUN cd /usr/local/bin \
##	&& mv R Rdevel \
##	&& mv Rscript Rscriptdevel \
##	&& ln -s Rdevel RD \
##	&& ln -s Rscriptdevel RDscript

## now we can pre-install a load of packages we know we'll need

RUN r -e "install.packages(c('devtools', 'XML', 'testthat', 'Rcpp', 'ggplot2', 'brew', \
	'rcolorbrewer', 'dichromat', 'munsell', 'checkmate', 'scales', 'proto', 'catools', \ 
	'evaluate', 'plyr', 'gtable', 'reshape2', 'knitr', 'microbenchmark', 'profr', 'xtable', \
	'rmarkdown'))"

# ENTRYPOINT ["check.r", "--install-deps"]
# ENTRYPOINT ["R"]
# CMD ["--help"]

# how to build myself:
# current directory at project root, i.e. containing Dockerfile and Makevars.site
# docker build -t r-clang-san .
