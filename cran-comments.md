Minor update, including a bug fix which could affect output accuracy.

# Test environments
* Ubuntu 18.04 R 3.5.1, GCC 7.3, clang 6
* Ubuntu 14.04 (Travis) R-devel, gcc
* Windows Server 2012 R2 x64 (Appveyor), R devel 32 and 64 bit
* MacOS High Sierra, brew clang 6
* winbuilder fails due to pandoc version error

# R CMD check results

There is one note:

* checking installed package size ... NOTE
  installed size is 22.3Mb
  sub-directories of 1Mb or more:
    R     15.2Mb
    doc    5.2Mb
    libs   1.1Mb
