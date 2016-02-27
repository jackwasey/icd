<!--
Copyright (C) 2014 - 2016  Jack O. Wasey

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
icd9
====

[![CRAN version](http://www.r-pkg.org/badges/version/icd9)](https://cran.r-project.org/package=icd9) [![Build Status](https://travis-ci.org/jackwasey/icd9.svg?branch=master)](https://travis-ci.org/jackwasey/icd9) [![Coverage Status](https://coveralls.io/repos/jackwasey/icd9/badge.svg?branch=master)](https://coveralls.io/r/jackwasey/icd9?branch=master) RStudio mirror: [![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/icd9)](https://cran.r-project.org/package=icd9) [![wercker status](https://app.wercker.com/status/158c843e8218371b23d1e44072770e92/m/master "wercker status")](https://app.wercker.com/project/bykey/158c843e8218371b23d1e44072770e92)

icd10: [![Build Status](https://travis-ci.org/jackwasey/icd9.svg?branch=icd10)](https://travis-ci.org/jackwasey/icd9) [![codecov.io](https://codecov.io/github/jackwasey/icd9/coverage.svg?branch=icd10)](https://codecov.io/github/jackwasey/icd9?branch=icd10) [![wercker status](https://app.wercker.com/status/158c843e8218371b23d1e44072770e92/m/icd10 "wercker status")](https://app.wercker.com/project/bykey/158c843e8218371b23d1e44072770e92)

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
patient_data %>% icd9LongToWide # everything works well with magrittr %>%
#>      [,1]    [,2]   [,3]   [,4]   
#> 1000 "40201" "2258" "7208" "25001"
#> 1001 "34400" "4011" NA     NA     
#> 1002 "4011"  NA     NA     NA

# get comorbidities:
icd9ComorbidQuanDeyo(patient_data)
#>         MI   CHF   PVD Stroke Dementia Pulmonary Rheumatic   PUD LiverMild
#> 1000 FALSE  TRUE FALSE  FALSE    FALSE     FALSE     FALSE FALSE     FALSE
#> 1001 FALSE FALSE FALSE  FALSE    FALSE     FALSE     FALSE FALSE     FALSE
#> 1002 FALSE FALSE FALSE  FALSE    FALSE     FALSE     FALSE FALSE     FALSE
#>         DM  DMcx Paralysis Renal Cancer LiverSevere  Mets   HIV
#> 1000  TRUE FALSE     FALSE FALSE  FALSE       FALSE FALSE FALSE
#> 1001 FALSE FALSE      TRUE FALSE  FALSE       FALSE FALSE FALSE
#> 1002 FALSE FALSE     FALSE FALSE  FALSE       FALSE FALSE FALSE

# find diagnoses present on admission:
icd9FilterPoa(patient_data)
#>   visit_id  icd9
#> 1     1000 40201
#> 4     1000 25001
#> 6     1001  4011
```

Note that reformatting from wide to long and back is not as straightforward as using the various Hadley Wickham tools for doing this: knowing the more detailed structure of the data let's us do this better for the case of dealing with ICD codes.

Install
-------

The latest version is available in [github](https://github.com/jackwasey/icd9) and can be installed with:

    install.packages("devtools") # if needed
    devtools::install_github("jackwasey/icd9")

    install.packages("magrittr") # recommended, but not required

The *master* branch at github should always build and pass all tests and R CMD check, and will be similar or identical to the most recent CRAN release. The CRAN releases are stable milestones. Contributions and bug reports are encouraged.

Advanced
--------

In the spirit of reproducible research, all the R data files in this package can be recreated from source. The size of the source files makes it cumbersome to include them in the R package available on CRAN. Using the [github](https://github.com/jackwasey/icd9) source, you can pull the source data, too, and rebuild the data, or use the tools to update the data using new source data files, e.g. when ICD-10-CM 2017 is released.

Doing the parsing requires additional dependencies, which are not gratuitously included in the package requirements, since most users won't need them. Benchmarking this package also has additional requirements. These are: - devtools - digest - ggplot2 - rmarkdown - readr - RSelenium - xml2 - xtable
