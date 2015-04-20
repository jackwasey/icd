## Note
* Thank you for comments on a previous (failed) submission of version 1.1 about the need for configure script to test independently for OpenMP and C++11, which is now done.

## Test environments
* Ubuntu 14.10 R 3.2, clang 3.7.0 and gcc 4.9.1
* Ubuntu 12.04 (on travis-ci) R 3.2, gcc 4.6.3
* Debian jessie in docker with UBSAN and clang, R 3.1.2
* Windows 7 64 bit R 3.2 with gcc from Rtools32

## R CMD check results

There is one note:

* checking data for non-ASCII characters ... NOTE
  Note: found 14 marked Latin-1 strings
  Note: found 39 marked UTF-8 strings

These are integral to included data which has accented characters in some disease names.
