Please note that a pandoc version error prevents the automated CRAN checks from passing on some platforms. The Pandoc version dependency for building the vignettes is in the DESCRIPTION.

This version of icd Suggests: version 1.1 of icd.data which I am also submitting.
It also Suggests: a new package 'nhds' which I am submitting.

# Test environments

  * Ubuntu 18.10 R 3.5.2, clang 7.0
  * Ubuntu 14.04 (Travis) R-devel, gcc
  * Windows Server 2012 R2 x64 (Appveyor), R devel 32 and 64 bit
  * MacOS High Sierra, brew clang 7.0

# CRAN checks
 
CRAN checks show warnings for Windows platforms due to pandoc version error when rebuilding vignettes.

# R CMD check results

Sometimes I get a URL download fail for the URL http://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD9-CM/2011/Dtab12.zip which is in a man page. The problem is that the http site directs to https sometimes, and then on some platforms has a certificate error. The URL is valid.

There is one note:

* checking installed package size ... NOTE
    installed size is  8.8Mb
    sub-directories of 1Mb or more:
      R      1.5Mb
      doc    5.5Mb
      libs   1.0Mb
