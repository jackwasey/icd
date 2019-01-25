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
<!-- README.md is generated from README.Rmd. Please edit that file and render with rmarkdown::render("README.Rmd")
-->

icd
===

[![CRAN](https://www.r-pkg.org/badges/version/icd "CRAN")](https://cran.r-project.org/package=icd)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![GitHub](https://img.shields.io/badge/devel%20version-3.3.9000-blue.svg?style=flat "GitHub")](https://github.com/jackwasey/icd)
[![Travis](https://travis-ci.org/jackwasey/icd.svg?branch=master "Travis Build Status")](https://travis-ci.org/jackwasey/icd)
[![Appveyor](https://ci.appveyor.com/api/projects/status/9ncfgxht3n5i8t60/branch/master?svg=true "Appveyor Build Status")](https://ci.appveyor.com/project/jackwasey/icd/branch/master)
[![codecov.io](https://codecov.io/github/jackwasey/icd/coverage.svg?branch=master "Core Code Coverage")](https://codecov.io/github/jackwasey/icd?branch=master)
[![CII Best
Practices](https://bestpractices.coreinfrastructure.org/projects/2092/badge)](https://bestpractices.coreinfrastructure.org/projects/2092)
[![CRAN RStudio mirror downloads last calendar
month](https://cranlogs.r-pkg.org/badges/icd "RStudio Mirror Downloads Last Calendar Month")](https://cran.r-project.org/package=icd)
[![CRAN RStudio mirror downloads last
week](https://cranlogs.r-pkg.org/badges/last-week/icd "RStudio Mirror Downloads Last Week")](https://cran.r-project.org/package=icd)

Comorbidities from ICD-9 and ICD-10 codes, manipulation and validation
======================================================================

Introduction
------------

Calculate comorbidities, Charlson and van Walraven scores, perform fast
and accurate validation, conversion, manipulation, filtering and
comparison of ICD-9 and ICD-10 codes. This package enables a work flow
from raw lists of ICD codes in hospital databases to comorbidities.
ICD-9 and ICD-10 comorbidity mappings from Quan (Deyo and Elixhauser
versions), Elixhauser and AHRQ included. Common ambiguities and code
formats are handled.

`icd` is used by many researchers around the world who work in public
health, epidemiology, clinical research, nutrition, journalism, health
administration and more. I’m grateful for contact from people in these
fields for their feedback and code contributions, and I’m pleased to say
that `icd` has been used in works like the [Pulitzer
finalist](http://www.pulitzer.org/finalists/staff-propublica) work on
[maternal death](http://www.pulitzer.org/finalists/staff-propublica) by
[ProPublica](https://www.propublica.org).

Features
--------

-   find comorbidities of patients based on ICD-9 or ICD-10 codes,
    e.g. Cancer, Heart Disease
    -   several standard mappings of ICD codes to comorbidities are
        included (Quan, Deyo, Elixhauser, AHRQ, PCCC)
    -   *very fast* assignment of ICD codes to comorbidities (using
        novel matrix multiplication algorithm and C++ internally)
-   use your existing data format, minimizing requirements for
    pre-processing
-   summarize groups of ICD codes in natural language
-   Charlson and Van Walraven score calculations
-   Hierarchical Condition Codes (HCC) from CMS
-   Clinical Classifcations Software (CCS) comorbidities from AHRQ
-   Pediatric Complex Chronic Condition comorbidities
-   AHRQ ICD-10 procedure code classification
-   annual revisions of ICD-9-CM and ICD-10-CM
-   correct conversion between different representations of ICD codes,
    with and without a decimal points, leading and trailing characters
    (this is not trivial for ICD-9-CM). ICD-9 to ICD-10 cross-walk is
    not yet implemented
-   comprehensive test suite to increase confidence in accurate
    processing of ICD codes
-   all internal ICD and comorbidity data is extracted directly from
    public data or code, allowing end-to-end reproducibility
-   used, tested and benchmarked against other comorbidity calculators
    on hardware from laptops to big servers

Examples
--------

See also the vignettes and examples embedded in the help for each
function for more. Here’s a taste:

``` r
# install.packages("icd")
library(icd)

# Typical diagnostic code data, with many-to-many relationship
patient_data
#>   visit_id  icd9
#> 1     1000 40201
#> 2     1000  2258
#> 3     1000  7208
#> 4     1000 25001
#> 5     1001 34400
#> 6     1001  4011
#> 7     1002  4011
#> 8     1000  <NA>

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

# plot summary of Uranium Cancer Registry sample data using AHRQ comorbidities
plot_comorbid(icd.data::uranium_pathology)
```

![](man/figures/README-example-1.png)

How to get help
---------------

Look at the help files for details and examples of almost every function
in this package. There are several vignettes showing the main features:

-   [Introduction](introduction.html)
-   [Charlson scores](charlson-scores.html)
-   [Examples using ICD-10 codes](ICD-10.html)
-   [CMS Hierarchical Condition Codes (HCC)](CMS-HCC.html)
-   [Pediatric Complex Chronic Conditions (PCCC)](PCCC.html)
-   [Working with ICD code ranges](ranges.html)
-   [Comparing comorbidity maps](compare-maps.html)
-   [Paper detailing efficient matrix method of
    comorbidities](efficiency.html)

Many users have emailed me directly for help, and I’ll do what I can,
but it is often better to examine or add to the list of
[issues](https://github.com/jackwasey/icd) so we can help each other.
Advanced users may look at the [source
code](https://github.com/jackwasey/icd), particularly the extensive test
suite which exercises all the key functions.

``` r
?comorbid
?comorbid_hcc
?explain_code
?is_valid

# first show the list
vignette(package = "icd")
vignette("introduction", package = "icd")
```

Relevance
---------

ICD-9 codes are still in heavy use around the world, particularly in the
USA where the ICD-9-CM (Clinical Modification) was in widespread use
until the end of 2015. ICD-10 has been used worldwide for reporting
cause of death for more than a decade, and ICD-11 is due to be released
in 2018. ICD-10-CM is now the primary coding scheme for US hospital
admission and discharge diagnoses used for regulatory purposes and
billing. A vast amount of electronic patient data is recorded with ICD-9
codes of some kind: this package enables their use in R alongside
ICD-10.

Comorbidities
-------------

A common requirement for medical research involving patients is
determining new or existing comorbidities. This is often reported in
*Table 1* of research papers to demonstrate the similarity or
differences of groups of patients. This package is focussed on fast and
accurate generation of this comorbidity information from raw lists of
ICD-9 codes.

ICD-9 codes
-----------

ICD-9 codes are not numbers, and great care is needed when matching
individual codes and ranges of codes. It is easy to make mistakes, hence
the need for this package. ICD-9 codes can be presented in *short* 5
character format, or *decimal* format, with a decimal place separating
the code into two groups. There are also codes beginning with V and E
which have different validation rules. Zeroes after a decimal place are
meaningful, so numeric ICD-9 codes cannot be used in most cases. In
addition, most clinical databases contain invalid codes, and even
decimal and non-decimal format codes in different places. This package
primarily deals with ICD-9-CM (Clinical Modification) codes, but should
be applicable or easily extendible to the original WHO ICD-9 system.

ICD-10 codes
------------

ICD-10 has a somewhat simpler format, with consistent use of a letter,
then two alphanumeric characters. However, especially for ICD-10-CM,
there are a multitude of qualifiers, e.g. specifying recurrence,
laterality, which vastly increase the number of possible codes. This
package recognizes validity of codes by syntax alone, or whether the
codes appear in a canonical list. The current ICD-10-CM master list is
the 2016 set. There is no capability of converting between ICD-9 and
ICD-10, but comorbidities can be generated from older ICD-9 codes and
newer ICD-10 codes in parallel, and the comorbidities can then be
compared.

Development version
-------------------

The latest version is available in [github
icd](https://github.com/jackwasey/icd), and can be installed with:

``` r
    #install.packages("remotes")
    remotes::install_github("jackwasey/icd")
```
