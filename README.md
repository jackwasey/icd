
icd9
====

ICD-9 code manipulation, validation and comorbidity generation in R
-------------------------------------------------------------------

ICD-9 codes are not numbers, and great care is needed when matching individual codes and ranges of codes. It is easy to make mistakes, hence the need for this package. ICD-9 codes can be presented in 'short' 5 character format, or 'decimal' format, with a decimal place seperating the code into two groups. There are also codes beginning with V and E which have different validation rules. Zeroes after a decimal place are meaningful, so numeric ICD-9 codes cannot be used in most cases. In addition, most clinical databases contain invalid codes, and even decimal and non-decimal format codes in different places. This package primarily deals with ICD-9-CM (Clinical Modification) codes, but should be applicable or easily extendible to the original WHO ICD-9 system. This package offers the following main features:

 * validation of ICD-9 codes (decimal or non-decimal "short" form), including V and E prefixes
 * conversion of ICD-9 codes between decimal and short forms
 * allocation of cases (e.g. patients or patient visits) to standard groups of co-morbidities according to ICD-9 coding and arbitrary mappings of ICD-9 codes to co-morbidities. AHRQ mapping is provided.
 * sorting of ICD-9 codes
 * generation of child codes from lists of higher-level ICD-9 codes, allowing testing of whether a specific ICD-9 code falls under a more general code category.

This package contains ICD-9 to co-morbidity mappings from several sources, based on either the Charlson or Elixhauser lists of co-morbidities. Updated versions of these lists from [AHRQ](http://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp) and [Quan et al](http://www.ncbi.nlm.nih.gov/pubmed/16224307) are included, along with the original Elixhauser mapping . Since some data is provided in SAS source code format, this package contains functions to parse this SAS source code and generate R data structures. This processing is limited to what is needed for this purpose, although may be generalizable and useful in other contexts. Other lists are transcribed directly from the published articles, but interpretation of SAS code from the publication author is preferred.

More detail can be found in the accompanying vignette, the thorough documentation of package functions, and by examination of the source code including the comprehensive unit tests. Suggestions for improvement and contributions of code are very welcome. Contributions will be credited. I ask that, if you use this package for published work, you cite this work citation(package='icd9')

The latest version is available in [github](https://github.com/jackwasey/icd9) and can be installed with:
`devtools::install_github('icd9', user="jackwasey")`
The master branch at github should always build and pass all tests and R CMD check. The CRAN releases are relatively stable milestones, but always somewhat behind the git repo. I want to minimize changes to the API from now on, but need feedback to confirm I have made good choices.

Further work
------------
There are a number of outstanding opportunities and problems. Enhancement and feature requests are tracked on [github](https://github.com/jackwasey/icd9/issues?state=open). Feel free to add to the list. The following are general areas of which could be worked upon, not yet tracked in github:
 * ICD-10-CM is imminent. This will most likely be a new package.
 * ICD-9 is not the same as ICD-9-CM (clinical modification). ICD-9 codes have only one decimal place, or four total digits. There is significant overlap, but this package is not intended to be used with ICD-9 codes. All top-level codes are the same, however.
 * performance is currently reasonable. There is a benchmark script which picks out some common use cases, and bottlenecks. It can allocate a million rows to a dozen comorbidities in less than ten seconds, on a modest workstation. Almost everything is vectorized, but some of the string processing could probably be done more quickly in C++. The test suite is fairly robust, so refactoring for performance shouldn't be too troublesome.
 * test coverage is only 70% at function resolution (see my fork of [testthat](https://github.com/jackwasey/testthat) for how to do this.)
 * additional testing against more real world ICD-9 data would be very helpful, and thereby generation of new tests.
 * the codes would best be presented in an object oriented model. This would be a significant refactoring but would have some advantages, most notable dispatch on long/short codes, self validation (which could be done once per instantiation, if S5 objects are used), and overall would be a more pleasing, although more complicated way of presenting the interface to a user. S3 classes might be preferable, making the base icd9 class 'short' form, then print.icd9 could interpret the code into a human-readable description, for example. Low-level compare method would then allow use of sort(). Not sure this has a lot of use cases, beyond interpreting the comorbidity definitions, which already works fine. Using an OO model would, however, place more burden on the package user. Perhaps the two approaches could co-exist and share a lot of code.
 * many functions have been written and documented, but I have been conservative in exposing them to the end-user with `@export`. The user can access these functions using the `icd9:::` syntax, or by forking and exporting. The code is approaching feature completeness, and once I have some feedback on suitability of the API in the real world, I'll begin to expose the mature functions.
 
