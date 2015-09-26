## Emacs, make this -*- mode: sh; -*-

## start with the Docker 'base R' Debian-based image
FROM rocker/r-devel-ubsan-clang

# FROM HERE IS ICD9 specific

## I think that, although this seems like a good idea, once R-dev is installed, it would need many or all of these to be rebuilt, since the debian ones are just built against some debian archive version of R.
## RUN apt-get update -y && apt-get install -y -t unstable libxml2-dev libssl-dev r-base-core r-cran-xml r-cran-ggplot2 r-cran-rcurl r-cran-bitops r-cran-brew r-cran-rcolorbrewer r-cran-rcpp r-cran-dichromat r-cran-munsell r-cran-checkmate r-cran-evaluate r-cran-plyr r-cran-gtable r-cran-reshape2 r-cran-scales r-cran-proto r-cran-catools r-cran-testthat r-cran-memoise r-cran-digest r-cran-xtable

## do install these pesky libraries which must come from unstable (sid), and not the testing repo which is the default.
RUN apt-get update -y && apt-get install -y -t unstable \
	libxml2-dev/unstable \
	libssl-dev/unstable \
	libcurl4-openssl-dev \
	texlive-base \
	fonts-inconsolata \
	git \
	libssh2-1-dev \
	qpdf \
	pandoc \
    	pandoc-citeproc


## now we can pre-install a load of packages we know we'll need

# use Rdevel (from the rocker image, because R is the standard g++ unsanitized one)
RUN Rdevel -e "install.packages(c('devtools', 'XML', 'testthat', 'Rcpp', 'ggplot2', 'brew', 'rcolorbrewer', 'dichromat', 'munsell', 'checkmate', 'scales', 'proto', 'catools', 'evaluate', 'plyr', 'gtable', 'reshape2', 'knitr', 'microbenchmark', 'profr', 'xtable', 'rmarkdown', 'fastmatch'))"

ADD build_and_test_icd9.sh /tmp/
CMD ["/tmp/build_and_test_icd9.sh"]
