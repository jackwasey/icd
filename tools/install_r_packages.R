# used by the docker containers to share common code:

# install.packages(c("backports"), INSTALL_opts="--no-byte-compile")

install.packages(c('codetools', 'devtools', 'testthat', 'Rcpp', 'rcolorbrewer',
                   'dichromat', 'munsell', 'checkmate', 'scales', 'proto',
                   'catools', 'evaluate', 'plyr', 'knitr', 'microbenchmark',
                   'profr', 'xtable', 'rmarkdown', 'fastmatch', 'roxygen2',
                   'RODBC', 'xml2'),
                 INSTALL_opts = "--no-byte-compile")
