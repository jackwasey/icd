## Note
* Thank you for comments on previous (failed) submission of version 1.1 about the need for configure script to test independently for OpenMP and C++11. This is now done. I realise this is a little soon after previous release, but it provides important speed improvements and bug fixes, and will be the last for a good while.

## Test environments
* Ubuntu 14.10 R 3.1.3, clang 3.7.0 and gcc 4.9.1
* Ubuntu 12.04 (on travis-ci) R 3.1.3, gcc 4.6.3
* Debian jessie in docker with UBSAN and clang, R 3.1.2
* win-builder (R-devel 64 bit execution fails with hang in Rf_error(), discussed with Duncan Murdoch, likely toolchain/Rcpp interaction on win-builder)
* Windows 7 64 bit R 3.1.3 with gcc from Rtools32

## R CMD check results

There are four notes:

 checking CRAN incoming feasibility ... NOTE
 Days since last update: 3

 This initial submission was in the end was not accepted, hence this resubmission.

 Possibly mis-spelled words in DESCRIPTION:
  AHRQ (9:62)
  Comorbidities (2:56)
  Deyo (9:16)
  Elixhauser (9:25, 9:47)
  ICD (2:31, 4:71, 5:36, 7:61, 8:50)
  Quan (9:10)
  comorbidities (3:38, 8:35)
  comorbidity (8:59)

"Comorbidity" is in widespread use in published medical literature, whereas "co-morbidity" is rarely seen.
 
 * checking data for non-ASCII characters ... NOTE
  Note: found 7 marked UTF-8 strings

These are integral to included data which has accented characters in some disease names.
  
* checking installed package size ... NOTE
  installed size is  6.9Mb
  sub-directories of 1Mb or more:
    extdata   3.4Mb
    libs      1.9Mb

Total package size is less than 1 Mb tar.gz

