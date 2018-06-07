With more users of this package, a bug was revealed which could affect accuracy of results, so please accept this new version.

# Test environments
* Ubuntu 18.04 R 3.5.0, GCC 7.3, clang 6
* Ubuntu 14.04 (Travis) R-devel, gcc
* Debian in docker with R-devel, ASAN+UBSAN GCC 7.3, clang 6
* Windows Server 2012 R2 x64 (Appveyor), R devel 32 and 64 bit
* OSX High Sierra, clang 6

# R CMD check results

There is one note:

* checking installed package size ... NOTE
  installed size is  9.7Mb
  sub-directories of 1Mb or more:
    R     5.4Mb
    doc   3.5Mb
