## Test environments
* Ubuntu 14.10 R 3.1.3, clang and gcc
* Ubuntu 12.04 (on travis-ci) R 3.1.2
* Debian jessie in docker with UBSAN and clang, R 3.1.2
* win-builder (R-devel 64 bit execution fails with hang in Rf_error(), discussed with Duncan Murdoch, likely toolchain/Rcpp interaction on win-builder)
* Windows 7 64 bit R 3.1.3 with gcc from Rtools32

## R CMD check results

There are three notes:
 
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

