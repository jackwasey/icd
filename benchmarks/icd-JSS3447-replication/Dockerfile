FROM rocker/tidyverse:latest
RUN mkdir /icd-JSS3447-repl/
WORKDIR /icd-JSS3447-repl/
COPY install-dependencies.R .
COPY checkpoint.R .
RUN mkdir ~/.R
RUN mkdir .checkpoint
RUN echo 'CFLAGS=-w' > ~/.R/Makevars
RUN echo 'CXXFLAGS=-w' >> ~/.R/Makevars
RUN echo 'CXX11FLAGS=-w' >> ~/.R/Makevars
#RUN echo 'CFLAGS=-march=native -O0 -pipe -w' > ~/.R/Makevars
#RUN echo 'CXXFLAGS=-march=native -O0 -pipe -w' >> ~/.R/Makevars
#RUN echo 'CXX11FLAGS=-march=native -O0 -pipe -w' >> ~/.R/Makevars
#RUN echo 'CXX14FLAGS=-march=native -O0 -pipe -w' >> ~/.R/Makevars
#RUN Rscript install-dependencies.R
COPY bench-versus.R .
COPY Makefile .
RUN make deps
CMD make 

