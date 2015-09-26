## Emacs, make this -*- mode: sh; -*-

## start with the local build, until up on docker hub
FROM r-devel-ubsan-clang-3.7-libcxx

# FROM HERE IS ICD9 specific

## do install these pesky libraries which must come from unstable (sid), and not the testing repo which is the default.
RUN apt-get update -y && apt-get dist-upgrade -y && apt-get install -y -t unstable \
	fonts-inconsolata \
	git \
	libcurl4-openssl-dev \
	libssl-dev/unstable \
	libxml2-dev/unstable \
	libssh2-1-dev \
	pandoc \
    	pandoc-citeproc \
	qpdf \
	texlive-base


# use R (I didn't build my image to use Rdevel)
RUN R -e "install.packages(c('devtools', 'XML', 'testthat', 'Rcpp', 'ggplot2', 'brew', 'rcolorbrewer', 'dichromat', 'munsell', 'checkmate', 'scales', 'proto', 'catools', 'evaluate', 'plyr', 'gtable', 'reshape2', 'knitr', 'microbenchmark', 'profr', 'xtable', 'rmarkdown', 'fastmatch'))"

ADD build_and_test_icd9.sh /tmp/
CMD ["/tmp/build_and_test_icd9.sh"]
