# Resubmission

Updated DESCRIPTION date and moved credits from documentation to DESCRIPTION, as requested on previous CRAN submission.

# SAN error 
I think the error shown at https://www.stats.ox.ac.uk/pub/bdr/memtests/gcc-ASAN/icd/ is a false positive. I can trigger the error by inserting some faulty code which triggers exactly the same sanitizer error, so I think my environment is capable of detecting this particular type of error, if one exists. I used all the config fragments available to set up the test environment, except your tests are run under gcc 7.1 and I used 7.2. In addition clang-trunk sanitizer does not detect this error.

# Test environments
* Ubuntu 17.04 R 3.4.1, clang 4.0, clang-trunk
* Ubuntu 12.04 (on travis-ci) R-devel, gcc
* Debian in docker with R-devel, ASAN+UBSAN GCC 7.2, clang 4.0 & trunk
* Windows 7, Rtools 3.4, 64-bit biarch and 32-bit, R-3.4 and R-devel
* Appveyor Windows Server 2012 R2 x64, R 3.4.1-patched 32 and 64 bit

# R CMD check results

On win64 only, I get the false positive NOTE with R Under development (unstable) (2017-09-02 r73196):
File 'icd/libs/x64/icd.dll':
  Found no calls to: 'R_registerRoutines', 'R_useDynamicSymbols'

There is one other note:

checking installed package size ... NOTE
  installed size is  6.3Mb
  sub-directories of 1Mb or more:
    data   1.3Mb
    doc    1.2Mb
    libs   2.9Mb
