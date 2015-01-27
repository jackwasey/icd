---
output:
  md_document:
    variant: markdown_github
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

# icd9

master [![Build Status](https://travis-ci.org/jackwasey/icd9.svg?branch=master)](https://travis-ci.org/jackwasey/icd9) [![Coverage Status](https://coveralls.io/repos/jackwasey/icd9/badge.svg?branch=master)](https://coveralls.io/r/jackwasey/icd9?branch=master)

_dev_ [![Build Status](https://travis-ci.org/jackwasey/icd9.png?branch=rcpp-optim)](https://travis-ci.org/jackwasey/icd9) 
[![Coverage Status](https://coveralls.io/repos/jackwasey/icd9/badge.svg?branch=rcpp-optim)](https://coveralls.io/r/jackwasey/icd9?branch=rcpp-optim)

# ICD-9 comorbidities, manipulation and validation

Calculate comorbidities, and perform fast and accurate validation, conversion, manipulation, filtering and comparison of ICD-9-CM (clinical modification) codes. ICD-9 codes appear numeric but leading and trailing zeroes, and both decimal and non-decimal "short" format codes exist. The package enables a work flow from raw lists of ICD-9 codes from hospital billing databases to comorbidities. ICD-9 to comorbidity mappings from Quan (Deyo and Elixhauser versions), Elixhauser and AHRQ included.

ICD-9 codes are still in wide use around the world, particularly in the USA where the ICD-9-CM (Clinical Modification) is in widespread use. ICD-10 and the corresponding ICD-10-CM are imminent, however a vast amount of patient data is recorded with ICD-9 codes of some kind: this package enables their use in R. A common requirement for medical research involving patients is determining new or existing comorbidities. This often get reported in *Table 1* of research papers to demonstrate the similarity of groups. This package is focussed on fast and accurate generation of this comorbidity information from raw lists of ICD-9 codes.

ICD-9 codes are not numbers, and great care is needed when matching individual codes and ranges of codes. It is easy to make mistakes, hence the need for this package. ICD-9 codes can be presented in *short* 5 character format, or *decimal* format, with a decimal place separating the code into two groups. There are also codes beginning with V and E which have different validation rules. Zeroes after a decimal place are meaningful, so numeric ICD-9 codes cannot be used in most cases. In addition, most clinical databases contain invalid codes, and even decimal and non-decimal format codes in different places. This package primarily deals with ICD-9-CM (Clinical Modification) codes, but should be applicable or easily extendible to the original WHO ICD-9 system.

## Main Features

 * assignment of patients to high level comorbidities based on admission or discharge ICD-9 codes
     * several mappings of ICD-9 codes to comorbidities are included (Quan, Deyo, Elixhauser, AHRQ)
     * very fast assignment of ICD-9 codes to comorbidities (using C++ internally)
 * validation of ICD-9 codes
 * summarizing ICD-9 codes into groups, and to human-readable descriptions
 * conversion between different representations of ICD-9 codes, with and without a decimal point
 * comprehensive tests to increase confidence in accurate processing of ICD-9 codes.

## Examples

See the vignette for many more examples. Here's a taste, 


```r
patientData
#>   visitId  icd9  poa
#> 1    1000 40201    Y
#> 2    1000  2258 <NA>
#> 3    1000  7208    N
#> 4    1000 25001    Y
#> 5    1001 34400    X
#> 6    1001  4011    Y
#> 7    1002  4011    E

# get comorbidities:
icd9ComorbidQuanDeyo(patientData)
#>   visitId    MI   CHF   PVD Stroke Dementia Pulmonary Rheumatic   PUD
#> 1    1000 FALSE  TRUE FALSE  FALSE    FALSE     FALSE     FALSE FALSE
#> 2    1001 FALSE FALSE FALSE  FALSE    FALSE     FALSE     FALSE FALSE
#> 3    1002 FALSE FALSE FALSE  FALSE    FALSE     FALSE     FALSE FALSE
#>   LiverMild    DM  DMcx Paralysis Renal Cancer LiverSevere  Mets   HIV
#> 1     FALSE  TRUE FALSE     FALSE FALSE  FALSE       FALSE FALSE FALSE
#> 2     FALSE FALSE FALSE      TRUE FALSE  FALSE       FALSE FALSE FALSE
#> 3     FALSE FALSE FALSE     FALSE FALSE  FALSE       FALSE FALSE FALSE

# find diagnoses present on admission:
icd9FilterPoa(patientData)
#>   visitId  icd9
#> 1    1000 40201
#> 4    1000 25001
#> 6    1001  4011

# reformat input data:
patientData %>% icd9LongToWide # everything works well with magrittr
#>   visitId icd_01 icd_02 icd_03 icd_04
#> 1    1000  40201   2258   7208  25001
#> 2    1001  34400   4011   <NA>   <NA>
#> 3    1002   4011   <NA>   <NA>   <NA>
```


## Install

The latest version is available in [github](https://github.com/jackwasey/icd9) and can be installed with:

    # install.packages("devtools") # if needed
    devtools::install_github("jackwasey/icd9")

    install.packages("magrittr") # recommended, but not required

The _master_ branch at github should always build and pass all tests and R CMD check, and will be similar or identical to the most recent CRAN release. The CRAN releases are stable milestones. Contributions and bug reports are encouraged.
