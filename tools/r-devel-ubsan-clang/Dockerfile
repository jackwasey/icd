## Emacs, make this -*- mode: sh; -*-

## start with the Docker 'base R' Debian-based image
# n.b. the github repo is actually called r-devel-san-clang
FROM rocker/r-devel-ubsan-clang

# openmp is disabled in this image

## do install these pesky libraries which must come from unstable (sid), and not the testing repo which is the default.
RUN apt-get update -y && apt-get dist-upgrade -y && apt-get install -y -t unstable \
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

## now we can pre-install a load of packages we know we'll need, or alternatively, devtools can do this based on the DESCRIPTION file once the package source is available (but this wouldn't be preserved in the docker image)

# use Rdevel (from the rocker image, because R is the standard g++ unsanitized one)
RUN Rdevel -e "install.packages(c('devtools', 'XML', 'testthat', 'Rcpp', 'ggplot2', 'brew', 'rcolorbrewer', 'dichromat', 'munsell', 'checkmate', 'scales', 'proto', 'catools', 'evaluate', 'plyr', 'gtable', 'reshape2', 'knitr', 'microbenchmark', 'profr', 'xtable', 'rmarkdown', 'fastmatch'))"

ADD build_and_test_icd9.sh /tmp/
CMD ["/tmp/build_and_test_icd9.sh"]
