# Test environments
* Ubuntu 18.04 R 3.5.0, GCC 7.3, clang 5.0.1
* Ubuntu 14.04 (Travis) R-devel, gcc
* Debian in docker with R-devel, ASAN+UBSAN GCC 7.3, clang 5.0.1
* Windows Server 2012 R2 x64 (Appveyor), R 3.4.3 32 and 64 bit
* OSX High Sierra, clang via brew

# R CMD check results

There is one note:

* checking installed package size ... NOTE
  installed size is  9.3Mb
  sub-directories of 1Mb or more:
    R      1.6Mb
    data   2.0Mb
    doc    3.8Mb
    libs   1.3Mb
