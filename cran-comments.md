## Note
* More thanks to Prof. Ripley for further advice on an obscure memory access violation. This version fixes bug introduced by version 1.2.1, and removes OpenMP support which seems to be at the root of problems which I am currently unable to replicate.

## Test environments
* Ubuntu 15.04 R 3.2.2, clang 3.6.0 and gcc 4.9.2
* Ubuntu 12.04 (on travis-ci) R 3.2, gcc 4.6.3
* Debian jessie in docker with R-devel Address and UBSAN and clang 3.7, R 3.2.2
* Windows 7 64 bit R 3.2 with gcc from Rtools32
* Windows 7 64 bit R 3.3 devel with gcc from Rtools33
* OS X with R-devel and LLVM Clang 3.7 (no OpenMP...)

# R CMD check results

There is one note:

* checking data for non-ASCII characters ... NOTE
  Note: found 14 marked Latin-1 strings
  Note: found 39 marked UTF-8 strings

These are integral to included data which have accented characters in some disease names.
