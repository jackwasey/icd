## Note
* Removed stringr dependency, thus resolves stringr-related errors, as requested.

## Test environments
* Ubuntu 16.04 R 3.3.1, clang 3.9 and gcc 5.3.1
* Ubuntu 14.04 (on travis-ci) R 3.2.4 patched, gcc 4.6.3
* Wercker Debian (rocker/hadleyverse) GCC 5.3.1, R 3.2.4 patched
* Debian testing/unstable in docker with R-devel, ASAN+UBSAN GCC, 3.8, 3.9 and trunk

# R CMD check results

There are two notes:

* checking installed package size ... NOTE
  installed size is 13.0Mb
  sub-directories of 1Mb or more:
    data   1.9Mb
    doc    2.2Mb
    libs   7.8Mb

The included data is maximally compressed with xz, and the source package is
only 2.8M.

* checking data for non-ASCII characters ... NOTE
  Note: found 14 marked Latin-1 strings
  Note: found 96 marked UTF-8 strings

These are required to describe disease names with accented characters correctly.
