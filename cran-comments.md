## Test environments
* Ubuntu 17.04 R 3.4.1, clang 4.0, clang-trunk
* Ubuntu 12.04 (on travis-ci) R-devel, gcc
* Debian in docker with R-devel, ASAN+UBSAN GCC 7.1, clang 4.0 & trunk
* Windows 7, Rtools 3.4, 64-bit biarch and 32-bit, R-3.4 and R-devel
* Appveyor Windows Server 2012 R2 x64, R 3.4.1-patched 32 and 64 bit

# R CMD check results

There is one note:

checking installed package size ... NOTE
  installed size is  6.3Mb
  sub-directories of 1Mb or more:
    data   1.3Mb
    doc    1.2Mb
    libs   2.9Mb
