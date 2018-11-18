Minor update, including a bug fix which could affect output accuracy.

# Test environments
* Ubuntu 18.10 R 3.5.1, GCC 8.2, clang 7.0.0
* Ubuntu 14.04 (Travis) R-devel, gcc
* Windows Server 2012 R2 x64 (Appveyor), R devel 32 and 64 bit
* MacOS High Sierra, brew clang 7.0.0
* winbuilder fails due to pandoc version error

# CRAN checks
 
CRAN checks show warnings for Windows platforms due to pandoc version error when rebuilding vignettes.

# R CMD check results

Sometimes I get a URL download fail for the URL http://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD9-CM/2011/Dtab12.zip which is in a man page. The problem is that the http site directs to https sometimes, and then on some platforms has a certificate error. The URL is valid.

There is one note:

* checking installed package size ... NOTE
  installed size is 22.0Mb
  sub-directories of 1Mb or more:
    R     15.2Mb
    doc    5.0Mb
    libs   1.0Mb
