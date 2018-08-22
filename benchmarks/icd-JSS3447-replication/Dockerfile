FROM rocker/tidyverse:latest
COPY install-dependencies.R /
RUN mkdir ~/.R
RUN echo 'CXXFLAGS+=-march=native -O0 -pipe -Wno-ignored-attributes' > ~/.R/Makevars
RUN echo 'CXX11FLAGS+=-march=native -O0 -pipe -Wno-ignored-attributes' >> ~/.R/Makevars
RUN MAKEFLAGS=-j5 Rscript install-dependencies.R
COPY bench-versus.R /
COPY Makefile /
CMD MAKEFLAGS=-j5 make
