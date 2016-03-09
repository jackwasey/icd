## Note
* This package now handles both ICD-9 and ICD-10 codes, as it was formerly called icd9, I would like to rename it to
  'icd'.

## Test environments
* Ubuntu 15.10 R 3.2.3, clang 3.7.0 and gcc 4.9.2 (4.9.2-10ubuntu13)
* Ubuntu 12.04 (on travis-ci) R 3.2.2, gcc 4.6.3
* Wercker
* Debian testing/unstable in docker with R-devel, ASAN+UBSAN GCC, clang 3.6, 3.7, 3.8-head
* Windows 7 64 bit R 3.3 devel with gcc from latest Rtools33
* OS X with R-devel and LLVM Clang 3.7

# R CMD check results

There is one note:

* checking data for non-ASCII characters ... NOTE
  Note: found 14 marked Latin-1 strings
  Note: found 39 marked UTF-8 strings

These are integral to included data which have accented characters in some disease names.
