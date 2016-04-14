## Note
* No longer warns when the deprecated package 'icd9' is installed.

## Test environments
* Ubuntu 16.04 R 3.2.4, clang 3.8.0 and gcc 5.3.1
* Ubuntu 14.04 (on travis-ci) R 3.2.4 patched, gcc 4.6.3
* Wercker Debian (rocker/hadleyverse) GCC 5.3.1, R 3.2.4 patched
* Debian testing/unstable in docker with R-devel, ASAN+UBSAN GCC, 3.8 and 3.9 trunk

# R CMD check results

There are four notes:

* checking installed package size ... NOTE
  installed size is 13.0Mb
  sub-directories of 1Mb or more:
    data   1.9Mb
    doc    2.2Mb
    libs   7.8Mb

The included data is maximally compressed with xz, and the source package is
only 2.8M.

Found the following (possibly) invalid URLs:
  URL: https://cran.r-project.org/package=icd
    From: README.md
    Status: 404
    Message: Not Found

This will be valid when accepted by CRAN.

* checking data for non-ASCII characters ... NOTE
  Note: found 14 marked Latin-1 strings
  Note: found 96 marked UTF-8 strings

These are required to describe disease names with accented characters correctly.

* checking DESCRIPTION meta-information ... NOTE
'LinkingTo' for ‘testthat’ is unused as it has no 'include' directory

The forthcoming 'testthat' package does have an include directory. 
I use the 'configure' script to identify its presence and act accordingly.
