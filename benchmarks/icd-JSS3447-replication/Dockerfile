FROM rocker/tidyverse:latest
RUN mkdir /icd-JSS3447-repl/
WORKDIR /icd-JSS3447-repl/
COPY install-dependencies.R .
RUN mkdir ~/.R
RUN echo 'CFLAGS=-w' > ~/.R/Makevars
RUN echo 'CXXFLAGS=-w' >> ~/.R/Makevars
RUN echo 'CXX11FLAGS=-w' >> ~/.R/Makevars
COPY bench-versus.R .
COPY Makefile .
COPY comorbidity_0.1.1.tar.gz .
COPY medicalrisk_1.2.tar.gz .
RUN make deps
CMD make result7
