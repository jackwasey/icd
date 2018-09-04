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
RUN make deps
CMD make result7
