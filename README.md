<!--
Copyright (C) 2014 - 2018  Jack O. Wasey

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

[![CRAN](https://www.r-pkg.org/badges/version/icd "CRAN")](https://cran.r-project.org/package=icd) [![GitHub](https://img.shields.io/badge/devel%20version-3.2.0.9000-blue.svg?style=flat "GitHub")](https://github.com/jackwasey/icd) [![Travis](https://travis-ci.org/jackwasey/icd.svg?branch=master "Travis Build Status")](https://travis-ci.org/jackwasey/icd) [![Appveyor](https://ci.appveyor.com/api/projects/status/9ncfgxht3n5i8t60/branch/master?svg=true "Appveyor Build Status")](https://ci.appveyor.com/project/jackwasey/icd/branch/master) [![codecov.io](https://codecov.io/github/jackwasey/icd/coverage.svg?branch=master "Core Code Coverage")](https://codecov.io/github/jackwasey/icd?branch=master) [![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/icd "RStudio Mirror Downloads")](https://cran.r-project.org/package=icd)

Comorbidities from ICD-9 and ICD-10 codes, manipulation and validation
======================================================================

Features
--------

-   find comorbidities of patients based on admission or discharge ICD-9 or ICD-10 codes, e.g. Cancer, Heart Disease
    -   several standard mappings of ICD codes to comorbidities are included (Quan, Deyo, Elixhauser, AHRQ, PCCC)
    -   very fast assignment of ICD codes to comorbidities (using matrix multiplication with C and C++ internally)
-   summarizing groups of ICD codes in natural language
-   Charlson and Van Walraven score calculations
-   Hierarchical Condition Codes (HCC) from CMS
-   Clinical Classifcations Software (CCS) comorbidities from AHRQ
-   Pediatric Complex Chronic Condition comorbidities
-   AHRQ ICD-10 procedure code classification
-   validation of ICD codes from different annual revisions of ICD-9-CM and ICD-10-CM
-   correct conversion between different representations of ICD codes, with and without a decimal points, leading and trailing characters (this is not trivial for ICD-9-CM). ICD-9 to ICD-10 cross-walk is not yet implemented
-   comprehensive test suite to increase confidence in accurate processing of ICD codes
-   all internal ICD and comorbidity data is extracted directly from publically available data or code, increasing confidence in the results

Install
-------

``` r
install.packages("icd")
```

Introduction
------------

Calculate comorbidities, Charlson scores, perform fast and accurate validation, conversion, manipulation, filtering and comparison of ICD-9 and ICD-10 codes. This package enables a work flow from raw lists of ICD codes in hospital databases to comorbidities. ICD-9 and ICD-10 comorbidity mappings from Quan (Deyo and Elixhauser versions), Elixhauser and AHRQ included. Common ambiguities and code formats are handled.

Relevance
---------

ICD-9 codes are still in heavy use around the world, particularly in the USA where the ICD-9-CM (Clinical Modification) was in widespread use until the end of 2015. ICD-10 has been used worldwide for reporting cause of death for more than a decade, and ICD-11 is due to be released in 2018. ICD-10-CM is now the primary coding scheme for US hospital admission and discharge diagnoses used for regulatory purposes and billing. A vast amount of electronic patient data is recorded with ICD-9 codes of some kind: this package enables their use in R alongside ICD-10.

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

# get comorbidities using Quan's application of Deyo's Charlson comorbidity groups
comorbid_charlson(patient_data)
#>         MI   CHF   PVD Stroke Dementia Pulmonary Rheumatic   PUD LiverMild
#> 1000 FALSE  TRUE FALSE  FALSE    FALSE     FALSE     FALSE FALSE     FALSE
#> 1001 FALSE FALSE FALSE  FALSE    FALSE     FALSE     FALSE FALSE     FALSE
#> 1002 FALSE FALSE FALSE  FALSE    FALSE     FALSE     FALSE FALSE     FALSE
#>         DM  DMcx Paralysis Renal Cancer LiverSevere  Mets   HIV
#> 1000  TRUE FALSE     FALSE FALSE  FALSE       FALSE FALSE FALSE
#> 1001 FALSE FALSE      TRUE FALSE  FALSE       FALSE FALSE FALSE
#> 1002 FALSE FALSE     FALSE FALSE  FALSE       FALSE FALSE FALSE

# or go straight to the Charlson scores:
charlson(patient_data)
#> 1000 1001 1002 
#>    2    2    0

# get comorbidities based on present-on-arrival diagnoses, use magrittr to flow the data
patient_data %>% filter_poa %>% comorbid_elix
#>        CHF Arrhythmia Valvular  PHTN   PVD   HTN Paralysis NeuroOther
#> 1000 FALSE      FALSE    FALSE FALSE FALSE FALSE     FALSE      FALSE
#> 1001 FALSE      FALSE    FALSE FALSE FALSE  TRUE     FALSE      FALSE
#>      Pulmonary    DM  DMcx Hypothyroid Renal Liver   PUD   HIV Lymphoma
#> 1000     FALSE  TRUE FALSE       FALSE FALSE FALSE FALSE FALSE    FALSE
#> 1001     FALSE FALSE FALSE       FALSE FALSE FALSE FALSE FALSE    FALSE
#>       Mets Tumor Rheumatic Coagulopathy Obesity WeightLoss FluidsLytes
#> 1000 FALSE FALSE     FALSE        FALSE   FALSE      FALSE       FALSE
#> 1001 FALSE FALSE     FALSE        FALSE   FALSE      FALSE       FALSE
#>      BloodLoss Anemia Alcohol Drugs Psychoses Depression
#> 1000     FALSE  FALSE   FALSE FALSE     FALSE      FALSE
#> 1001     FALSE  FALSE   FALSE FALSE     FALSE      FALSE
```

Look at the help files for details and examples of almost every function in this package.

``` r
?comorbid
?comorbid_hcc
?explain_code
?is_valid
```

Note that reformatting from wide to long and back is not as straightforward as using the various Hadley Wickham tools for doing this: knowing the more detailed structure of the data let's us do this better for the case of dealing with ICD codes.

Advanced
--------

### Source Data and SAS format files

In the spirit of reproducible research, all the R data files in this package can be recreated from source. The size of the source files makes it cumbersome to include them in the R package available on CRAN. Using the github source, you can pull the original data and SAS format files, and rebuild the data; or use the tools provided by this package to update the data using new source data files, e.g. when ICD-10-CM 2017 is released.

### Development version

The latest version is available in [github icd](https://github.com/jackwasey/icd), and can be installed with:

``` r
    install.packages("devtools")
    devtools::install_github("jackwasey/icd")
```

The *master* branch at github should always build and pass all tests and R CMD check, and will be similar or identical to the most recent CRAN release. The CRAN releases are stable milestones. Contributions and bug reports are encouraged and essential for this package to remain current and useful to the many people who have installed it.

### Contributing and Building

A substantial amount of code has now been contributed to the package. Contributions of any kind to `icd` are very welcome. See the \[GitHub issues page\]\](<https://github.com/jackwasey/icd/issues>) to see jobs and feature requests. Documentation, vignettes and examples are very welcome, especially if accompanied by some real-world data.

To build `icd`, `Rcpp` must be compiled from source. This happens automatically on Linux, but on Mac and Windows, the following may be required: `install.packages("Rcpp", type="source")`
