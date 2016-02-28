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

## start with the local build, until up on docker hub
FROM r-devel-ubsan-clang-3.7-libcxx

## install some libraries and supporting binaries, some of which must come from unstable (sid), and not the testing repo which is the default in the r-devel base rocker image.
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
RUN R -e "install.packages(c('devtools', 'testthat', 'Rcpp', 'rcolorbrewer', 'dichromat', 'munsell', 'checkmate', 'scales', 'proto', 'catools', 'evaluate', 'knitr', 'microbenchmark', 'profr', 'rmarkdown', 'fastmatch'))"

# can only add files inside the docker directory: the following script is copied from parent directory by rocker-icd9.sh
ADD build_install_check_in_docker.sh /tmp/
CMD ["/tmp/build_install_check_in_docker.sh"]
