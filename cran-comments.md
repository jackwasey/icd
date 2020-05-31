Re-submission, with corrected links in documentation, and package sized reduced under 5 Mb as requested by Dr. Ligges. With apologies for inability to fix the check warnings for compiler flags and documentation. I have now Removed deprecated CPP flag as requested. Fixed new documentation-related warnings from CRAN. Thanks for your work.

# Test environments

  * Ubuntu 19.04 R 3.6.3, clang 9, gcc 9, clang 10, gcc 10
  * Travis CI: MacOS, Ubuntu 16.04, 18.04 (Travis) R-devel, R-release, gcc, clang
  * Windows Server 2012 R2 x64 (Appveyor), R devel 32 and 64 bit
  * MacOS Mojave 10.14.16, R 4.0.0, brew clang 9

# R CMD check results

Sometimes I get a URL download fail for the URL http://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD9-CM/2011/Dtab12.zip which is in a man page. The problem is that the http site directs to https sometimes, and then on some platforms has a certificate error. The URL is valid.

There is one note:

* checking data for non-ASCII characters ... NOTE
  Note: found 20 marked UTF-8 strings
  
These are from accented characters in the US disease name definitions for ICD-9-CM and ICD-10-CM.
