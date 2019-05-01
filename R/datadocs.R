#' ICD-9 chapters
#'
#' \code{icd9_chapters}, \code{icd9_chapters_sub} and \code{icd9_majors} contain
#' mappings from the higher level descriptions of ICD-9 codes to the ranges of
#' ICD-9 codes they describe. Helpful in summarizing codes or grouping for
#' human-readable output. These can easily be converted to a co-morbidity
#' mapping, as shown in the vignette.
#'
#' \itemize{ \item 001-139 Infectious And Parasitic Diseases \item 140-239
#' Neoplasms \item 240-279 Endocrine, Nutritional And Metabolic Diseases, And
#' Immunity Disorders \item 280-289 Diseases Of The Blood And Blood-Forming
#' Organs \item 290-319 Mental Disorders \item 320-389 Diseases Of The Nervous
#' System And Sense Organs \item 390-459 Diseases Of The Circulatory System
#' \item 460-519 Diseases Of The Respiratory System \item 520-579 Diseases Of
#' The Digestive System \item 580-629 Diseases Of The Genitourinary System \item
#' 630-679 Complications Of Pregnancy, Childbirth, And The Puerperium \item
#' 680-709 Diseases Of The Skin And Subcutaneous Tissue \item 710-739 Diseases
#' Of The Musculoskeletal System And Connective Tissue \item 740-759 Congenital
#' Anomalies \item 760-779 Certain Conditions Originating In The Perinatal
#' Period \item 780-799 Symptoms, Signs, And Ill-Defined Conditions \item
#' 800-999 Injury And Poisoning \item V01-V91 Supplementary Classification Of
#' Factors Influencing Health Status And Contact With Health Services \item
#' E000-E999 Supplementary Classification Of External Causes Of Injury And
#' Poisoning }
#' @keywords datasets list category
#' @docType data
#' @source
# nolint start
#' \url{http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html}
# nolint end
#'
#' @details Format: list with chapter/sub-chapter or major names stored in list
#'   names, each with two element named character vector with start and end
#'   codes.
#' @name icd9_chapters
#' @aliases icd9_majors
NULL

#' ICD-9 sub-chapters
#' @name icd9_sub_chapters
#' @seealso \code{\link{icd9_chapters}}
NULL

#' ICD-10 chapters
#'
#' The WHO ICD-10 scheme chapters. The chapter level is the highest in the
#' hierarchy, each chapter containing sets of codes which span multiple
#' three-digit 'major' codes, and in some cases also span codes across two
#' alphabetic initial characters. E.g. Chapter I spans A00 to B99.
#'
#' 2017 ICD-10-CM does not have any U codes (codes for special purposes).
#' U00-U49 - Provisional assignment of new diseases of uncertain etiology or
#' emergency use U82-U85 - Resistance to antimicrobial and anti-neoplastic drugs
#' @source http://apps.who.int/classifications/icd10/browse/2016/en
#' @details Format: list with chapter names stored in list names, each with two
#'   element named character vector with start and end codes.
#' @name icd10_chapters
#' @aliases icd10_chapters_fr
#' @seealso \code{\link{icd10_sub_chapters}}
#' @docType data
#' @keywords datasets
NULL

#' ICD-10 sub-chapters
#'
#' The WHO ICD-10 scheme sub-chapters. N.b. there may be WHO vs CM differences:
#' please file bug if noted. In the \code{XML} definition of ICD-10-CM there are
#' some intermediate hierarchical levels, e.g. for neoplasms. Sub-chapter here
#' is defined as the lowest-level grouping of three-digit codes, e.g. C00-C14
#' "Malignant neoplasms of lip, oral cavity and pharynx", not C00-C96 "Malignant
#' neoplasms" which itself is a subset of the chapter C00-D49 "Neoplasms"
#' @source http://apps.who.int/classifications/icd10/browse/2016/en
#' @details Format: list with sub-chapter or major names stored in list names,
#'   each with two element named character vector with start and end codes.
#' @seealso \code{\link{icd10_chapters}}
#' @name icd10_sub_chapters
#' @aliases icd10_sub_chapters_fr
NULL

#' ICD-9-CM diagnosis codes including leaf nodes and branch names up to the
#' three-digit codes.
#'
#' Unlike \code{\link{get_icd9cm2014_leaf}} and friends, these data frames
#' contain the full structure of the ICD-9-CM scheme up to the three-digit
#' codes. Unlike the equivalent ICD-10-CM data frames (e.g.,
#' \code{\link{icd10cm2019}}), they do not have columns indicating billable/leaf
#' status, or chapter designations.
#'
#' \code{icd9cm_hierarchy} is the deprecated name, which currently points to the
#' final ICD-9-CM 2014 release. 2011 -- 2014 are in fact identical. These are
#' generated dynamically by parsing an awkward RTF file, and only the 2011 --
#' 2014 data has been tested and used thoroughly.
#' @docType data
#' @keywords datasets
#' @details Format:  data frame
#' @source
#' \url{http://wonder.cdc.gov/wonder/sci_data/codes/icd9/type_txt/icd9cm.asp}
#' @source Rich text descriptions here:
#'   \url{http://www.cdc.gov/nchs/icd/icd9cm.htm}
# nolint start
#'   \url{http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html}
# nolint end
#'    This page has versions 23 to 32 (2005 to 2014). At present, only the 2014
#'   data is included in this package.
#' @source
#' \url{http://wonder.cdc.gov/wonder/sci_data/codes/icd9/type_txt/icd9abb.asp}
#' @source
#' \url{http://wonder.cdc.gov/wonder/sci_data/codes/icd9/type_txt/icd9cm.asp}
#' @source
#' \url{http://wonder.cdc.gov/wonder/sci_data/codes/icd9/type_txt/icdcm.asp}
#' @source
#' \url{http://wonder.cdc.gov/wonder/sci_data/codes/icd9/type_txt/icd9abb.asp}
#' @name icd9cm_hierarchy
NULL

#' United States and Belgium ICD-10-CM
#'
#' The public domain modified ICD-10 classification as published in the public
#' domain by the US CDC. Currently this has a slightly different structure to
#' \code{icd9cm_hierarchy} because the published data helpfully has a
#' \emph{leaf} flag indicating whether a code is a \emph{billable} leaf node, or
#' a code higher in the hierarchy which nevertheless will have a description.
#' @section Editions: There are annual revisions to this data in the US.
#' @aliases icd10cm2016
#' @seealso \code{\link{get_icd10cm2014}} \code{\link{get_icd10cm2015}}
#'   \code{\link{get_icd10cm2017}} \code{\link{get_icd10cm2018}}
#' @source \url{http://www.cdc.gov/nchs/icd/icd10cm.htm}
#' @docType data
#' @keywords datasets
#' @details Format:  data frame, with columns for code, leaf status (0 or 1),
#'   short and long descriptions.
#' @references \url{https://www.cms.gov/Medicare/Coding/ICD10/}
# nolint start
#'   \url{https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2018-ICD-10-PCS-Tables-And-Index.zip}
#'   \url{https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2018-ICD-10-PCS-Order-File.zip}
#'   \url{https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2017-PCS-Code-Tables.zip}
#'   \url{https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2017-PCS-Long-Abbrev-Titles.zip}
#'   \url{https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2016-Code-Descriptions-in-Tabular-Order.zip}
#'   \url{https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2015-code-descriptions.zip}
#'   \url{https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2015-tables-index.zip}
#'   \url{https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2015-Code_Tables-and-Index.zip}
#'   \url{https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2015-PCS-long-and-abbreviated-titles.zip}
#'   \url{https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2014-ICD10-Code-Descriptions.zip}
#'   \url{https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2014-ICD10-Code-Tables-and-Index.zip}
#'   \url{https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2014-Code-Tables-and-Index.zip}
#'   \url{https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2014-PCS-long-and-abbreviated-titles.zip}
# nolint end
#' @name icd10cm2019
NULL

#' @name get_icd10cm2014
#' @title ICD-10-CM 2014
#' @seealso \code{\link{icd10cm2019}}
#' @docType data
#' @keywords datasets
#' @export
NULL

#' @name get_icd10cm2015
#' @title ICD-10-CM 2015
#' @seealso \code{\link{icd10cm2019}}
#' @docType data
#' @keywords datasets
#' @export
NULL

#' @name get_icd10cm2017
#' @title ICD-10-CM 2017
#' @seealso \code{\link{icd10cm2019}}
#' @docType data
#' @keywords datasets
#' @export
NULL

#' @name get_icd10cm2018
#' @title ICD-10-CM 2018
#' @seealso \code{\link{icd10cm2019}}
#' @docType data
#' @keywords datasets
#' @export
NULL

#' 2016 WHO ICD-10 data
#'
#' This data must be downloaded on a per-user basis. A prompt is given when the
#' data is first attempted to be accessed.
#' @name get_icd10who2016
#' @source \url{http://www.who.int}
#' @docType data
#' @keywords datasets
#' @export
NULL

#' 2008 WHO ICD-10 data in French
#'
#' This data must be downloaded on a per-user basis. A prompt is given when the
#' data is first attempted to be accessed.
#' @name get_icd10who2008fr
#' @source \url{http://www.who.int}
#' @docType data
#' @keywords datasets
#' @export
NULL

#' French ICD-10-FR modification of WHO ICD-10 used in France
#'
#' La Classification internationale statistique des maladies (CIM), version 10,
#' edition française (The International Classification of Diseases (ICD),
#' version 10, French edition.) Comme la version américaine, l'édition française
#' a beaucoup de changéements par rapport à l'édition de l'OMS. Juste l'année
#' 2018 est présentée pour le moment.
# "
#' The short descriptions are capitalized, and, as is correct in French, do not
#' require accents. These were not converted to lower or sentence case to avoid
#' introducing spelling errors.
#' Définitions CIM-10-FR de l'OMS (WHO ICD-10 definitions)
#' @source \url{https://www.atih.sante.fr/cim-10-fr-2018-usage-pmsi}
#' @references
# nolint start
#'   \href{https://www.atih.sante.fr/nomenclatures-de-recueil-de-linformation/cim}{ATIH
#'   CIM-10-FR}
# nolint end
#' @docType data
#' @keywords datasets
#' @name get_icd10fr2019
#' @export
NULL

#' Belgian ICD-10-BE
#'
#' This is based heavily on ICD-10-CM. 2014 is identical, with translations for
#' most of the codes into Dutch and French. 2017 has about a hundred additional
#' code definitions over ICD-10-CM 2017. The 2014 data also has the interesting
#' fields for gender specificity of a given code, and whether it is permissible
#' as Present-on-Arrival (POA).
#' @source
#' \url{https://www.health.belgium.be/en/node/30433}
# nolint start
#' \url{https://www.health.belgium.be/sites/default/files/uploads/fields/fpshealth_theme_file/fy2017_reflist_icd-10-be.xlsx_last_updatet_28-07-2017_1.xlsx}
#' \url{https://www.health.belgium.be/fr/sante/organisation-des-soins-de-sante/hopitaux/systemes-denregistrement/icd-10-be}
# nolint end
#' \url{https://www.health.belgium.be/fr/fy2014reflisticd-10-bexlsx}
#' @name get_icd10be2014
#' @seealso \code{\link{get_icd10be2017}} \code{\link{get_icd10be2014_pc}}
#'   \code{\link{get_icd10be2017_pc}}
#' @docType data
#' @keywords datasets
#' @export
NULL

#' @title ICD-10-BE 2017
#' @name get_icd10be2017
#' @seealso \code{\link{get_icd10be2014}} \code{\link{get_icd10be2014_pc}}
#'   \code{\link{get_icd10be2017_pc}}
#' @docType data
#' @keywords datasets
#' @export
NULL

#' @title ICD-10-BE 2017 procedure codes
#' @name get_icd10be2017_pc
#' @seealso \code{\link{get_icd10be2014}} \code{\link{get_icd10be2014_pc}}
#'   \code{\link{get_icd10be2017}}
#' @docType data
#' @keywords datasets
#' @export
NULL

#' @title ICD-10-BE 2014 procedure codes
#' @name get_icd10be2014_pc
#' @seealso \code{\link{get_icd10be2014}} \code{\link{get_icd10be2017}}
#'   \code{\link{get_icd10be2017_pc}}
#' @docType data
#' @keywords datasets
#' @export
NULL

#' ICD-10-CM Procedure Codes
#'
#' ICD-10-PCS is the annually-updated set of procedure codes designed by 3M for
#' the US CMS. There is no directory of WHO ICD procedure codes.
#' @format A named list of data frames. The elements of the list are named by
#'   the year, e.g., \code{"2018"}. Each data frame contains two character
#'   columns, the first, named \code{code} is the procedure code; the second,
#'   named \code{desc}, has the description.
#' @name get_icd10cm2019_pc
#' @seealso \code{get_icd10cm2014_pc} \code{get_icd10cm2015_pc}
#'   \code{get_icd10cm2016_pc} \code{get_icd10cm2017_pc}
#'   \code{get_icd10cm2018_pc}
#' \url{https://www.cms.gov/Medicare/Coding/ICD10/downloads/pcs_refman.pdf}
#' @docType data
#' @keywords datasets
#' @export
NULL

#' ICD-10-CM Procedure codes for 2014
#' @name get_icd10cm2014_pc
#' @seealso \code{get_icd10cm2015_pc} \code{get_icd10cm2016_pc}
#'   \code{get_icd10cm2017_pc} \code{get_icd10cm2018_pc}
#'   \code{get_icd10cm2019_pc}
#' @docType data
#' @keywords datasets
#' @export
NULL

#' ICD-10-CM Procedure codes for 2015
#' @name get_icd10cm2015_pc
#' @seealso \code{get_icd10cm2014_pc} \code{get_icd10cm2016_pc}
#'   \code{get_icd10cm2017_pc} \code{get_icd10cm2018_pc}
#'   \code{get_icd10cm2019_pc}
#' @docType data
#' @keywords datasets
#' @export
NULL

#' ICD-10-CM Procedure codes for 2016
#' @name get_icd10cm2016_pc
#' @seealso \code{get_icd10cm2014_pc} \code{get_icd10cm2015_pc}
#'   \code{get_icd10cm2017_pc} \code{get_icd10cm2018_pc}
#'   \code{get_icd10cm2019_pc}
#' @docType data
#' @keywords datasets
#' @export
NULL

#' ICD-10-CM Procedure codes for 2017
#' @name get_icd10cm2017_pc
#' @seealso \code{get_icd10cm2014_pc} \code{get_icd10cm2015_pc}
#'   \code{get_icd10cm2016_pc} \code{get_icd10cm2018_pc}
#'   \code{get_icd10cm2019_pc}
#' @docType data
#' @keywords datasets
#' @export
NULL

#' ICD-10-CM Procedure codes for 2018
#' @name get_icd10cm2018_pc
#' @seealso \code{get_icd10cm2014_pc} \code{get_icd10cm2015_pc}
#'   \code{get_icd10cm2016_pc} \code{get_icd10cm2017_pc}
#'   \code{get_icd10cm2019_pc}
#' @docType data
#' @keywords datasets
#' @export
NULL

#' ICD-9-CM, just billable/leaf codes
#'
#' @details These are derived from the final CMS published version 32 for 2014,
#'   which was unchanged since 2011. The short descriptions are in ASCII with no
#'   special characters, whereas the long descriptions contain accented
#'   characters which are stored as Unicode, \code{latin-1} or \code{cp1252}.
#'
#'   This all done during package creation, but can be repeated by package
#'   users, including pulling the data from the web pages directly. Despite my
#'   best efforts, current locale can give different results, but this packaged
#'   data is correct, with some \code{UTF-8} encoded strings.
#'   \code{icd9cm_billable} has been removed, and is replaced by now includes
#'   only the latest version (32).
#' @docType data
#' @keywords datasets
#' @format data frames with columns \code{code}, \code{short_desc}, and
#'   \code{long_desc}.
#' @source
# nolint start
#' \url{http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html}
# nolint end
#' @name get_icd9cm2014_leaf
#' @aliases iget_cd9cm2005_leaf get_icd9cm2006_leaf get_icd9cm2007_leaf
#'   get_icd9cm2008_leaf get_icd9cm2009_leaf get_icd9cm2010_leaf
#'   get_icd9cm2011_leaf get_icd9cm2012_leaf get_icd9cm2013_leaf
#'   get_icd9cm2014_leaf
#' @export
NULL

#' The final ICD-9-CM list of leaf (\sQuote{billable}) codes
#'
#' Other years are available from \code{\link{get_icd9cm2013_leaf}}, etc..
#' @name icd9cm2014_leaf
#' @docType data
#' @keywords datasets
NULL

#' @name get_icd9cm2005
#' @rdname icd9cm_hierarchy
#' @export
NULL

#' @name get_icd9cm2006
#' @rdname icd9cm_hierarchy
#' @export
NULL

#' @name get_icd9cm2007
#' @rdname icd9cm_hierarchy
#' @export
NULL

#' @name get_icd9cm2008
#' @rdname icd9cm_hierarchy
#' @export
NULL

#' @name get_icd9cm2009
#' @rdname icd9cm_hierarchy
#' @export
NULL

#' @name get_icd9cm2010
#' @rdname icd9cm_hierarchy
#' @export
NULL

#' @name get_icd9cm2011
#' @rdname icd9cm_hierarchy
#' @export
NULL

#' @name get_icd9cm2012
#' @rdname icd9cm_hierarchy
#' @export
NULL

#' @name get_icd9cm2013
#' @rdname icd9cm_hierarchy
#' @export
NULL

#' @name get_icd9cm2014
#' @rdname icd9cm_hierarchy
#' @export
NULL

#' @name get_icd9cm2005_leaf
#' @rdname get_icd9cm2014_leaf
#' @export
NULL

#' @name get_icd9cm2006_leaf
#' @rdname get_icd9cm2014_leaf
#' @export
NULL

#' @name get_icd9cm2007_leaf
#' @rdname get_icd9cm2014_leaf
#' @export
NULL

#' @name get_icd9cm2008_leaf
#' @rdname get_icd9cm2014_leaf
#' @export
NULL

#' @name get_icd9cm2009_leaf
#' @rdname get_icd9cm2014_leaf
#' @export
NULL

#' @name get_icd9cm2010_leaf
#' @rdname get_icd9cm2014_leaf
#' @export
NULL

#' @name get_icd9cm2011_leaf
#' @rdname get_icd9cm2014_leaf
#' @export
NULL

#' @name get_icd9cm2012_leaf
#' @rdname get_icd9cm2014_leaf
#' @export
NULL

#' @name get_icd9cm2013_leaf
#' @rdname get_icd9cm2014_leaf
#' @export
NULL

#' Deprecated name for the list of annual ICD-9-CM billable/leaf descriptions
#'
#' This now just contains the final version (2014, version number 32). This is
#' actually the same from 2011 to 2014. Use \code{get_xxx} functions such as
#' \code{\link{get_icd9cm2014}} instead.
#' @docType data
#' @keywords datasets
#' @format list of data frames with columns \code{code}, \code{short_desc}, and
#'   \code{long_desc}, containing just the leaf (in USA, \sQuote{billable})
#'   codes, and their descriptions.
#' @seealso For 2005 -- 2014, the following functions provide ICD-9-CM data:
#'   \code{\link{get_icd9cm2009}} and \code{\link{get_icd9cm2013_leaf}}. The
#'   former includes three-digit \sQuote{major} down to the leaf/billable level,
#'   and any intermediate descriptors, e.g. if a four-digit code is not
#'   billable, and encompasses some five-digit codes, then the four digit code
#'   would appear in \code{\link{get_icd9cm2014}} but not
#'   \code{\link{get_icd9cm2014_leaf}}.
#' @source
# nolint start
#' \url{http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html}
# nolint end
#' @name icd9cm_billable
NULL

#' The latest available version of ICD-10-CM in this package
#' @details This is an active binding, so is exported explicitly
#' @docType data
#' @keywords datasets
#' @export
get_icd10cm_latest <- function() {
  get_icd10cm2019()
}


#' Hospital discharge data from Vermont
#'
#' Anonymous data from public Vermont source for 2013
#'
#' Conditions of Release Release of public use data is subject to the following
#' conditions, which the requestor agrees to upon accepting copies of the data:
#'
#' 1. The data may not be used in any manner that attempts to or does identify,
#' directly or indirectly, any individual patient or physician.
#'
#' 2. The requestor agrees to incorporate the following, or a substantially
#' similar, disclaimer in all reports or publications that include public use
#' data: \dQuote{Hospital discharge data for use in this study were supplied by
#' the Vermont Association of Hospitals and Health Systems-Network Services
#' Organization (VAHHS-NSO) and the Vermont Department of Banking, Insurance,
#' Securities and Health Care Administration (BISHCA). All analyses,
#' interpretations or conclusions based on these data are solely that of [the
#' requestor]. VAHHS-NSO and BISHCA disclaim responsibility for any such
#' analyses, interpretations or conclusions. In addition, as the data have been
#' edited and processed by VAHHS-NSO, BISHCA assumes no responsibility for
#' errors in the data due to coding or processing}
#' @source
# nolint start
#' \url{http://www.healthvermont.gov/health-statistics-vital-records/health-care-systems-reporting/hospital-discharge-data}
# nolint end
#' @details Format:  CSV original, minimally processed into R data frame.
#' @keywords datasets
#' @author Vermont Division of Health Care Administration
#' @docType data
#' @name vermont_dx
#' @noMd
NULL

#' United States Transuranium & Uranium Registries
#'
#' This is an ICD-10 data set (not ICD-10-CM) with mortality from the United
#' States Transuranium & Uranium Registries, published in the public domain.
#' @name uranium_pathology
#' @source \url{https://ustur.wsu.edu/about-us/}
#' @docType data
#' @keywords datasets
NULL
