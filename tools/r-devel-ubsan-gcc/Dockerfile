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

#FROM rocker/r-devel:latest
FROM jackwasey/r-devel-ubsan-gcc

# alternatively use littelr or devtools to install the current dependencies
RUN Rscript -e 'install.packages(c("Rcpp", "checkmate", "testthat", "devtools", "knitr", "microbenchmark", "magrittr", "digest", "rmarkdown", "fastmatch", "roxygen2", "RODBC"))'

# can only add files inside the docker directory: the following script is copied from parent directory by rocker-icd9.sh
ADD build_install_check_in_docker.sh /tmp/
ENV LD_PRELOAD /usr/lib/x86_64-linux-gnu/libasan.so.2
ENV R_CMD Rdevel
ENV GIT_BRANCH icd10
CMD ["/tmp/build_install_check_in_docker.sh"]

