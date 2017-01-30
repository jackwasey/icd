<!--
Copyright (C) 2014 - 2017  Jack O. Wasey

This file is part of icd.

icd is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

icd is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with icd. If not, see <http:#www.gnu.org/licenses/>.
-->
<!-- README.md is generated from README.Rmd. Please edit that file and render with 
rmarkdown::render("README.Rmd")
-->
icd
===

icd statistics, based on Rstudio mirror
---------------------------------------

[![CRAN version](https://www.r-pkg.org/badges/version/icd)](https://cran.r-project.org/package=icd) [![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/icd)](https://cran.r-project.org/package=icd) [![Build Status](https://travis-ci.org/jackwasey/icd.svg?branch=master)](https://travis-ci.org/jackwasey/icd) [![codecov.io](https://codecov.io/github/jackwasey/icd/coverage.svg?branch=master)](https://codecov.io/github/jackwasey/icd?branch=master) [![Coverage Status](https://coveralls.io/repos/github/jackwasey/icd/badge.svg?branch=master)](https://coveralls.io/github/jackwasey/icd?branch=master)

Old package icd9 statistics
---------------------------

[![CRAN version](https://www.r-pkg.org/badges/version/icd9)](https://cran.r-project.org/package=icd9) [![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/icd9)](https://cran.r-project.org/package=icd9)

ICD-9 and ICD-10 comorbidities, manipulation and validation
===========================================================

Features
--------

-   find comorbidities of patients based on admission or discharge ICD-9 or ICD-10 codes, e.g. Cancer, Heart Disease
    -   several standard mappings of ICD codes to comorbidities are included (Quan, Deyo, Elixhauser, AHRQ)
    -   very fast assignment of ICD codes to comorbidities (using C and C++ internally, with automatic parallel execution using OpenMP when available), assigning millions of comorbidities in a few seconds
-   Charlson and Van Walraven score calculations
-   Hierarchical Condition Codes (HCC)
-   validation of ICD codes from different annual revisions of ICD-9-CM and ICD-10-CM
-   summarizing ICD codes into groups, and to human-readable descriptions
-   correct conversion between different representations of ICD codes, with and without a decimal points, leading and trailing characters (this is not trivial for ICD-9-CM). ICD-9 to ICD-10 conversion is left as an exercise for the user!
-   comprehensive test suite to increase confidence in accurate processing of ICD codes

Introduction
------------

Calculate comorbidities, Charlson scores, perform fast and accurate validation, conversion, manipulation, filtering and comparison of ICD-9-CM (clinical modification) codes. ICD-9 codes appear numeric but leading and trailing zeroes, and both decimal and non-decimal "short" format codes exist. The package enables a work flow from raw lists of ICD-9 codes from hospital billing databases to comorbidities. ICD-9 to comorbidity mappings from Quan (Deyo and Elixhauser versions), Elixhauser and AHRQ included. Any other mapping of codes, such as ICD-10, to comorbidities can be used.

Relevance
---------

ICD-9 codes are still in heavy use around the world, particularly in the USA where the ICD-9-CM (Clinical Modification) was in widespread use until the end of 2015. ICD-10 has been used worldwide for reporting cause of death for more than a decade. ICD-10-CM is now the primary coding scheme for US hospital admission and discharge diagnoses used for regulatory purposes and billing. A vast amount of patient data is recorded with ICD-9 codes of some kind: this package enables their use in R alongside ICD-10.

Comorbidities
-------------

A common requirement for medical research involving patients is determining new or existing comorbidities. This is often reported in *Table 1* of research papers to demonstrate the similarity or differences of groups of patients. This package is focussed on fast and accurate generation of this comorbidity information from raw lists of ICD-9 codes.

ICD-9 codes
-----------

ICD-9 codes are not numbers, and great care is needed when matching individual codes and ranges of codes. It is easy to make mistakes, hence the need for this package. ICD-9 codes can be presented in *short* 5 character format, or *decimal* format, with a decimal place separating the code into two groups. There are also codes beginning with V and E which have different validation rules. Zeroes after a decimal place are meaningful, so numeric ICD-9 codes cannot be used in most cases. In addition, most clinical databases contain invalid codes, and even decimal and non-decimal format codes in different places. This package primarily deals with ICD-9-CM (Clinical Modification) codes, but should be applicable or easily extendible to the original WHO ICD-9 system.

ICD-10 codes
------------

ICD-10 has a somewhat simpler format, with consistent use of a letter, then two alphanumeric characters. However, especially for ICD-10-CM, there are a multitude of qualifiers, e.g. specifying recurrence, laterality, which vastly increase the number of possible codes. This package recognizes validity of codes by syntax alone, or whether the codes appear in a canonical list. The current ICD-10-CM master list is the 2016 set. There is no capability of converting between ICD-9 and ICD-10, but comorbidities can be generated from older ICD-9 codes and newer ICD-10 codes in parallel, and the comorbidities can then be compared.

Examples
--------

See also the vignettes and examples embedded in the help for each function for more. Here's a taste:

``` r
patient_data
#>   visit_id  icd9  poa
#> 1     1000 40201    Y
#> 2     1000  2258 <NA>
#> 3     1000  7208    N
#> 4     1000 25001    Y
#> 5     1001 34400    X
#> 6     1001  4011    Y
#> 7     1002  4011    E

# reformat input data as needed
icd_long_to_wide(patient_data)
#>      [,1]    [,2]   [,3]   [,4]   
#> 1000 "40201" "2258" "7208" "25001"
#> 1001 "34400" "4011" NA     NA     
#> 1002 "4011"  NA     NA     NA

# get comorbidities using Quan's application of Deyo's Charlson comorbidity groups
icd_comorbid_quan_deyo(patient_data)
#>         MI   CHF   PVD Stroke Dementia Pulmonary Rheumatic   PUD LiverMild
#> 1000 FALSE  TRUE FALSE  FALSE    FALSE     FALSE     FALSE FALSE     FALSE
#> 1001 FALSE FALSE FALSE  FALSE    FALSE     FALSE     FALSE FALSE     FALSE
#> 1002 FALSE FALSE FALSE  FALSE    FALSE     FALSE     FALSE FALSE     FALSE
#>         DM  DMcx Paralysis Renal Cancer LiverSevere  Mets   HIV
#> 1000  TRUE FALSE     FALSE FALSE  FALSE       FALSE FALSE FALSE
#> 1001 FALSE FALSE      TRUE FALSE  FALSE       FALSE FALSE FALSE
#> 1002 FALSE FALSE     FALSE FALSE  FALSE       FALSE FALSE FALSE

# find diagnoses present on admission:
icd_filter_poa(patient_data)
#>   visit_id  icd9
#> 1     1000 40201
#> 4     1000 25001
#> 6     1001  4011

# get comorbidities based on present-on-arrival diagnoses, use magrittr to flow the data
patient_data %>% icd_filter_poa %>% icd_comorbid_quan_deyo
#>         MI   CHF   PVD Stroke Dementia Pulmonary Rheumatic   PUD LiverMild
#> 1000 FALSE  TRUE FALSE  FALSE    FALSE     FALSE     FALSE FALSE     FALSE
#> 1001 FALSE FALSE FALSE  FALSE    FALSE     FALSE     FALSE FALSE     FALSE
#>         DM  DMcx Paralysis Renal Cancer LiverSevere  Mets   HIV
#> 1000  TRUE FALSE     FALSE FALSE  FALSE       FALSE FALSE FALSE
#> 1001 FALSE FALSE     FALSE FALSE  FALSE       FALSE FALSE FALSE
```

Look at the help files for details and examples of almost every function in this package.

``` r
?icd_is_valid
?icd_comorbid
```

Note that reformatting from wide to long and back is not as straightforward as using the various Hadley Wickham tools for doing this: knowing the more detailed structure of the data let's us do this better for the case of dealing with ICD codes.

Install
-------

The latest version is available in [github icd](https://github.com/jackwasey/icd), and can be installed with:

``` r
    install.packages("devtools")
    devtools::install_github("jackwasey/icd")
```

The *master* branch at github should always build and pass all tests and R CMD check, and will be similar or identical to the most recent CRAN release. The CRAN releases are stable milestones. Contributions and bug reports are encouraged and essential for this package to remain current and useful to the many people who have installed it.

Advanced
--------

### Source Data and SAS format files

In the spirit of reproducible research, all the R data files in this package can be recreated from source. The size of the source files makes it cumbersome to include them in the R package available on CRAN. Using the github source, you can pull the original data and SAS format files, and rebuild the data; or use the tools provided by this package to update the data using new source data files, e.g. when ICD-10-CM 2017 is released.

Doing the parsing requires additional dependencies, which are not gratuitously included in the package requirements, since most users won't need them. Benchmarking this package also has additional requirements. These are: - xml2 - ggplot2 - digest

### Automated testing

One of the strengths of this package is a thorough test suite, including over 10,000 lines of testing code.

    find tests -type f -exec cat '{}' + | wc -l
    10910

A better metric of testing and code quality is code coverage, for which [codecov](https://codecov.io/github/jackwasey/icd) and [coveralls](https://coveralls.io/github/jackwasey/icd) are used. The automated [wercker](https://app.wercker.com/#applications/5609d41e71f137d02f0a1069) builds report test coverage results to [codecov](https://codecov.io/github/jackwasey/icd), whereas the [travis](https://travis-ci.org/jackwasey/icd) builds report coverage to [coveralls](https://coveralls.io/github/jackwasey/icd). The parsing code is a significant chunk of code, and may or may not be included in the automated builds depending on whether the source data is available. With the data available, test coverage is &gt;95%.
