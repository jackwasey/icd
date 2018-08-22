FROM rocker/tidyverse:latest
COPY . /
RUN echo 'options(repos = c(CRAN = "https://cran.rstudio.com"))' >> /usr/local/lib/R/etc/Rprofile.site
RUN mkdir ~/.R
RUN echo 'CXXFLAGS+=-march=native -flto -pipe -wno-ignored-attributes' >> ~/.R/Makevars
RUN MAKEFLAGS=-j4 Rscript install-dependencies.R
CMD MAKEFLAGS=-j4 make
