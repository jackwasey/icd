<!--
Copyright (C) 2014 - 2015  Jack O. Wasey

This file is part of icd9.

icd9 is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

icd9 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with icd9. If not, see <http:#www.gnu.org/licenses/>.
-->
<!-- README.md is generated from README.Rmd. Please edit that file and render with rmarkdown::render("README.Rmd")-->
Coming soon: `icd` to replace `icd9` which will support both ICD-9 and ICD-10
=============================================================================

See the [icd10 branch](https://github.com/jackwasey/icd9/tree/icd10) for current work. Soon, everything will be renamed to `icd` and new code will only be available in the new repo. The new package will have all of the functions currently available in `icd9`. Since function names mostly included `icd9` there was a lot of function renaming and deprecation, but `icd` will still be a drop-in replacement for `icd9`.

icd9
====

[![Build Status](https://travis-ci.org/jackwasey/icd9.svg?branch=master)](https://travis-ci.org/jackwasey/icd9) [![Coverage Status](https://coveralls.io/repos/jackwasey/icd9/badge.svg?branch=master)](https://coveralls.io/r/jackwasey/icd9?branch=master) [![CRAN version](http://www.r-pkg.org/badges/version/icd9)](https://cran.r-project.org/package=icd9) [![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/icd9)](https://cran.r-project.org/package=icd9) (RStudio mirror only)

ICD comorbidities, manipulation and validation
==============================================

Main Features
-------------

-   assignment of patients to high level comorbidities based on admission or discharge ICD-9 codes
    -   several mappings of ICD-9 codes to comorbidities are included (Quan, Deyo, Elixhauser, AHRQ)
    -   very fast assignment of ICD-9 codes to comorbidities (using C and C++ internally, with automatic parallel execution using OpenMP when available)
-   Charlson and Van Walvaren score calculations
-   validation of ICD-9 codes from different annual revisions of ICD-9-CM
-   summarizing ICD-9 codes into groups, and to human-readable descriptions
-   correct conversion between different representations of ICD-9 codes, with and without a decimal point
-   comprehensive test suite to increase confidence in accurate processing of ICD-9 codes.

New since last CRAN release:
----------------------------

-   further performance increases: 1 million ICD-9 codes assigned to comorbidities in less than a second
-   logical matrix or data.frame for comorbidity output and manipulation
-   see NEWS.md and github [changelog](https://github.com/jackwasey/icd9/commits/master) for more details
-   minor update to fix an obscure memory leak found with address sanitizer.

Work in progress:
-----------------

-   ICD-10 support is nearly complete. See the : contributions of tests, code or anything always welcome.

Introduction
------------

Calculate comorbidities, Charlson scores, perform fast and accurate validation, conversion, manipulation, filtering and comparison of ICD-9-CM (clinical modification) codes. ICD-9 codes appear numeric but leading and trailing zeroes, and both decimal and non-decimal "short" format codes exist. The package enables a work flow from raw lists of ICD-9 codes from hospital billing databases to comorbidities. ICD-9 to comorbidity mappings from Quan (Deyo and Elixhauser versions), Elixhauser and AHRQ included. Any other mapping of codes, such as ICD-10, to comorbidities can be used.

Relevance
---------

ICD-9 codes are still in heavy use around the world, particularly in the USA where the ICD-9-CM (Clinical Modification) is in widespread use. ICD-10 and the corresponding ICD-10-CM are imminent, however a vast amount of patient data is recorded with ICD-9 codes of some kind: this package enables their use in R. A common requirement for medical research involving patients is determining new or existing comorbidities. This is often reported in *Table 1* of research papers to demonstrate the similarity or differences of groups of patients. This package is focussed on fast and accurate generation of this comorbidity information from raw lists of ICD-9 codes.

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

# reformat input data as needed
patientData %>% icd9LongToWide # everything works well with magrittr %>%
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

Note that reformatting from wide to long and back is not as straightforward as using the various Hadley Wickham tools for doing this: knowing the more detailed structure of the data let's us do this better for the case of dealing with ICD codes.

Install
-------

The latest version is available in [github](https://github.com/jackwasey/icd9) and can be installed with:

    install.packages("devtools") # if needed
    devtools::install_github("jackwasey/icd9")

    install.packages("magrittr") # recommended, but not required

The *master* branch at github should always build and pass all tests and R CMD check, and will be similar or identical to the most recent CRAN release. The CRAN releases are stable milestones. Contributions and bug reports are encouraged.
