<!-- README.md is generated from README.Rmd. Please edit that file -->
icd9
====

[![Build Status](https://travis-ci.org/jackwasey/icd9.svg?branch=master)](https://travis-ci.org/jackwasey/icd9) [![Coverage Status](https://coveralls.io/repos/jackwasey/icd9/badge.svg?branch=master)](https://coveralls.io/r/jackwasey/icd9?branch=master)

ICD-9 comorbidities, manipulation and validation
================================================

Main Features
-------------

-   assignment of patients to high level comorbidities based on admission or discharge ICD-9 codes
    -   several mappings of ICD-9 codes to comorbidities are included (Quan, Deyo, Elixhauser, AHRQ)
    -   very fast assignment of ICD-9 codes to comorbidities (using C++ internally, with automatic parallel execution using OpenMP when possible)
-   Charlson and Van Walvaren score calculations
-   validation of ICD-9 codes from different annual revisions of ICD-9-CM
-   summarizing ICD-9 codes into groups, and to human-readable descriptions
-   correct conversion between different representations of ICD-9 codes, with and without a decimal point
-   comprehensive test suite to increase confidence in accurate processing of ICD-9 codes.

New since last CRAN release:
----------------------------

-   further performance increases: 1 million ICD-9 codes assigned to comorbidities in a couple of seconds
-   logical matrix or data.frame for comorbidity output and manipulation
-   see NEWS.md and github [changelog](https://github.com/jackwasey/icd9/commits/master) for more details

Introduction
------------

Calculate comorbidities, Charlson scores, perform fast and accurate validation, conversion, manipulation, filtering and comparison of ICD-9-CM (clinical modification) codes. ICD-9 codes appear numeric but leading and trailing zeroes, and both decimal and non-decimal "short" format codes exist. The package enables a work flow from raw lists of ICD-9 codes from hospital billing databases to comorbidities. ICD-9 to comorbidity mappings from Quan (Deyo and Elixhauser versions), Elixhauser and AHRQ included. Any other mapping of codes, such as ICD-10, to comorbidities can be used.

Relevance
---------

ICD-9 codes are still in wide use around the world, particularly in the USA where the ICD-9-CM (Clinical Modification) is in widespread use. ICD-10 and the corresponding ICD-10-CM are imminent, however a vast amount of patient data is recorded with ICD-9 codes of some kind: this package enables their use in R. A common requirement for medical research involving patients is determining new or existing comorbidities. This is often reported in *Table 1* of research papers to demonstrate the similarity or differences of groups of patients. This package is focussed on fast and accurate generation of this comorbidity information from raw lists of ICD-9 codes.

ICD-9 code types
----------------

ICD-9 codes are not numbers, and great care is needed when matching individual codes and ranges of codes. It is easy to make mistakes, hence the need for this package. ICD-9 codes can be presented in *short* 5 character format, or *decimal* format, with a decimal place separating the code into two groups. There are also codes beginning with V and E which have different validation rules. Zeroes after a decimal place are meaningful, so numeric ICD-9 codes cannot be used in most cases. In addition, most clinical databases contain invalid codes, and even decimal and non-decimal format codes in different places. This package primarily deals with ICD-9-CM (Clinical Modification) codes, but should be applicable or easily extendible to the original WHO ICD-9 system.

Examples
--------

See the vignette and code help for many more. Here's a taste:

``` r
patientData
#>   visitId  icd9  poa
#> 1    1000 40201    Y
#> 2    1000  2258 <NA>
#> 3    1000  7208    N
#> 4    1000 25001    Y
#> 5    1001 34400    X
#> 6    1001  4011    Y
#> 7    1002  4011    E

# reformat input data as needed:
patientData %>% icd9LongToWide # everything works well with magrittr
#>      [,1]    [,2]   [,3]   [,4]   
#> 1000 "40201" "2258" "7208" "25001"
#> 1001 "34400" "4011" NA     NA     
#> 1002 "4011"  NA     NA     NA

# get comorbidities:
icd9ComorbidQuanDeyo(patientData)
#>         MI   CHF   PVD Stroke Dementia Pulmonary Rheumatic   PUD LiverMild
#> 1000 FALSE  TRUE FALSE  FALSE    FALSE     FALSE     FALSE FALSE     FALSE
#> 1001 FALSE FALSE FALSE  FALSE    FALSE     FALSE     FALSE FALSE     FALSE
#> 1002 FALSE FALSE FALSE  FALSE    FALSE     FALSE     FALSE FALSE     FALSE
#>         DM  DMcx Paralysis Renal Cancer LiverSevere  Mets   HIV
#> 1000  TRUE FALSE     FALSE FALSE  FALSE       FALSE FALSE FALSE
#> 1001 FALSE FALSE      TRUE FALSE  FALSE       FALSE FALSE FALSE
#> 1002 FALSE FALSE     FALSE FALSE  FALSE       FALSE FALSE FALSE

# find diagnoses present on admission:
icd9FilterPoa(patientData)
#>   visitId  icd9
#> 1    1000 40201
#> 4    1000 25001
#> 6    1001  4011
```

Install
-------

The latest version is available in [github](https://github.com/jackwasey/icd9) and can be installed with:

    # install.packages("devtools") # if needed
    devtools::install_github("jackwasey/icd9")

    install.packages("magrittr") # recommended, but not required

The *master* branch at github should always build and pass all tests and R CMD check, and will be similar or identical to the most recent CRAN release. The CRAN releases are stable milestones. Contributions and bug reports are encouraged.
