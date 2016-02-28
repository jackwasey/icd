# Copyright (C) 2014 - 2016  Jack O. Wasey
#
# This file is part of icd.
#
# icd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd. If not, see <http:#www.gnu.org/licenses/>.

# Based heavily on rocker/r-devel-san and friends from Dirk Eddelbuettel.

FROM rocker/r-devel:latest

# use unstable to get the right development library versions
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

 RUN cd /tmp \
        && svn co http://svn.r-project.org/R/trunk R-devel

# GCC UBSAN and OpenMP don't work together in any version! appears to be no evidence of fix
# https://gcc.gnu.org/bugzilla/show_bug.cgi?id=64888

ENV ASAN_OPTIONS detect_leaks=0:detect_odr_violation=0

# build R-devel again, but with sanitizers
RUN cd /tmp/R-devel \
        && R_PAPERSIZE=letter \
           R_BATCHSAVE="--no-save --no-restore" \
           R_BROWSER=xdg-open \
           PAGER=/usr/bin/pager \
           PERL=/usr/bin/perl \
           R_UNZIPCMD=/usr/bin/unzip \
           R_ZIPCMD=/usr/bin/zip \
           R_PRINTCMD=/usr/bin/lpr \
           LIBnn=lib \
           AWK=/usr/bin/awk \
           CFLAGS="-g -Wall -pedantic -O2" \
           CXXFLAGS="-g -Wall -pedantic -O2" \
           CC="gcc -std=gnu99 -fsanitize=address,undefined -fno-omit-frame-pointer" \
           CXX="g++ -fsanitize=address,undefined  -fno-omit-frame-pointer -fno-sanitize=object-size,vptr" \
           CXX1X="g++ -fsanitize=address,undefined  -fno-omit-frame-pointer -fno-sanitize=object-size,vptr" \
           FC="gfortran -fsanitize=address" \
           F77="gfortran -fsanitize=address" \
	   FCFLAGS="-g -O0 -mtune=native" \
	   FFLAGS="-g -O0 -mtune=native" \
	   MAIN_LDFLAGS="-fsanitize=address,undefined" \
           ./configure --enable-R-shlib \
               --without-blas \
               --without-lapack \
	       --disable-openmp \
               --with-readline \
               --without-recommended-packages \
               --program-suffix=dev \
        && make -j5 \
        && make install \
        && make clean

# remove or keep -O0 flags? Not sure which would be most helpful. Most code in production use is compiled O2, but I don't know whether O0 would uncover the same UBSAN bugs, or just allow them to be characterized more easily. BDR used -O2 in the memtests he did on this package.

RUN Rscript -e 'install.packages(c("Rcpp", "checkmate", "testthat", "devtools", "knitr", "microbenchmark", "magrittr", "profr", "ggplot2", "digest", "xtable", "rmarkdown", "codetools", "fastmatch"))'

# can only add files inside the docker directory: the following script is copied from parent directory by rocker-icd9.sh
ADD build_install_check_in_docker.sh /tmp/
# set environment variables that I will use in the added script:
ENV LD_PRELOAD /usr/lib/x86_64-linux-gnu/libasan.so.2
ENV R_CMD Rdevel
CMD ["/tmp/build_install_check_in_docker.sh"]

