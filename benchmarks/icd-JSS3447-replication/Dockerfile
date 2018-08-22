FROM rocker/rstudio:latest
COPY . /
RUN echo 'options(repos = c(CRAN = "https://cran.rstudio.com"))' >> /usr/local/lib/R/etc/Rprofile.site 
RUN mkdir ~/.R
RUN echo 'CXX11FLAGS+=-march=native -flto -pipe -wno-ignored-attributes' >> ~/.R/Makeflags
RUN MAKEFLAGS=-j8 Rscript install-dependencies.R
CMD MAKEFLAGS=-j8 make

