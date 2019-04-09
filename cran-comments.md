I am submitting icd 4.0, icd.data 1.1.2 together. Thank you for comments on previous submission, all of which have been addressed. 'icd' and 'icd.data' versions need to be updated on CRAN together due to reverse dependency failure, but otherwise function regardless of the version of the other package, hence lack of version specifications in these DESCRIPTIONS.

Please note that a pandoc version error prevents the automated CRAN checks from passing on some platforms. The Pandoc version dependency for building the vignettes is in the DESCRIPTION.

# Test environments

  * Ubuntu 18.10 R 3.5.3, clang 7.0, gcc 8.2, 
  * Travis CI: MacOS, Ubuntu 16.04, 18.04 (Travis) R-devel, R-release, gcc, clang
  * Windows Server 2012 R2 x64 (Appveyor), R devel 32 and 64 bit
  * MacOS Mojave, R 3.5.3 brew clang 7.0, GCC-8.3, GCC-9.0.1-HEAD
  * R-hub (sanitizer, valgrind)

# CRAN checks
 
CRAN checks show warnings for Windows platforms due to pandoc version error when rebuilding vignettes.

# R CMD check results

Sometimes I get a URL download fail for the URL http://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD9-CM/2011/Dtab12.zip which is in a man page. The problem is that the http site directs to https sometimes, and then on some platforms has a certificate error. The URL is valid.

There is one note:

* checking installed package size ... NOTE
    installed size is  7.5Mb
    sub-directories of 1Mb or more:
      R      1.7Mb
      data   3.0Mb
      libs   2.2Mb
