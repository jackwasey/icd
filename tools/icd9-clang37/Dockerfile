FROM jw99
# or from clang37, clang38 etc

RUN Rscript -e 'install.packages(c("checkmate", "devtools", "testthat", "Rcpp"))'

RUN Rscript -e "library(devtools); install_github('jackwasey/icd9', branch = 'issue75')"
#RUN cd /tmp && git clone https://github.com/jackwasey/icd9.git && cd icd9 && git checkout issue75

CMD sh -c "cd /tmp && git clone https://github.com/jackwasey/icd9.git && cd icd9 && git checkout issue75 && Rscript -e \"library(devtools); library(testthat); setwd('/tmp/icd9'); load_all(); test_file('tests/testthat/test-openmp.R')\""

