# Copyright (C) 2014 - 2015  Jack O. Wasey
#
# This file is part of icd9.
#
# icd9 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd9 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd9. If not, see <http:#www.gnu.org/licenses/>.

#' @title ICD-9-CM chapters
#' @name icd9Chapters
#' @aliases icd9ChaptersSub icd9ChaptersMajor icd9chapters icd9Chapters
#' @description \code{icd9Chapters}, \code{icd9ChaptersSub} and
#'   \code{icd9ChaptersMajor} contain mappings from the higher level
#'   descriptions of ICD-9 codes to the ranges of ICD-9 codes they describe.
#'   Helpful in summarizing codes or grouping for human-readable output. These
#'   can easily be converted to a co-morbidity mapping, as shown in the
#'   vignette.
#'
#'   \itemize{ \item 001-139 Infectious And Parasitic Diseases \item 140-239
#'   Neoplasms \item 240-279 Endocrine, Nutritional And Metabolic Diseases, And
#'   Immunity Disorders \item 280-289 Diseases Of The Blood And Blood-Forming
#'   Organs \item 290-319 Mental Disorders \item 320-389 Diseases Of The Nervous
#'   System And Sense Organs \item 390-459 Diseases Of The Circulatory System
#'   \item 460-519 Diseases Of The Respiratory System \item 520-579 Diseases Of
#'   The Digestive System \item 580-629 Diseases Of The Genitourinary System
#'   \item 630-679 Complications Of Pregnancy, Childbirth, And The Puerperium
#'   \item 680-709 Diseases Of The Skin And Subcutaneous Tissue \item 710-739
#'   Diseases Of The Musculoskeletal System And Connective Tissue \item 740-759
#'   Congenital Anomalies \item 760-779 Certain Conditions Originating In The
#'   Perinatal Period \item 780-799 Symptoms, Signs, And Ill-Defined Conditions
#'   \item 800-999 Injury And Poisoning \item V01-V91 Supplementary
#'   Classification Of Factors Influencing Health Status And Contact With Health
#'   Services \item E000-E999 Supplementary Classification Of External Causes Of
#'   Injury And Poisoning }
#' @keywords datasets list category
#' @docType data
#' @source \url{http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html}
#' @format list with chapter/usb-chapter or major names stored in list names,
#'   each with two element named character vector with start and end codes.
NULL

#' @title ICD9-CM diagnosis code lookup
#' @description short-form ICD-9 codes with short and long descriptions, and
#'   description of each hierarchy level containing each code.
#' @docType data
#' @keywords datasets
#' @format data frame
#' @source
#' \url{http://wonder.cdc.gov/wonder/sci_data/codes/icd9/type_txt/icd9cm.asp}
#' @source Rich text descriptions here:
#'   \url{http://www.cdc.gov/nchs/icd/icd9cm.htm}
#'   \url{http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html}
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
#' @name icd9Hierarchy
NULL

#' @title AHRQ comorbidities
#' @description This mapping of comorbidities to ICD-9 codes is dervied directly
#'   from SAS code provided by AHRQ, and translated into this R data structure.
#'   This is a revision of the Elixhauser system, notably exluding cardiac
#'   arrythmia.
#' @docType data
#' @keywords datasets
#' @format list of character vectors
#' @source
#' \url{http://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp}
#' @name ahrqComorbid
NULL

#' @title AHRQ comorbidities, with HTN, CHF and renal failure subgroups
#' @description This mapping of comorbidities to ICD-9 codes is dervied directly
#'   from SAS code provided by AHRQ, and translated into this R data structure.
#'   Beyond ahrqComorbid, this includes all the HTN, CHF and renal subgroups,
#'   not rolled into their parent categories. This resolution is not needed in
#'   typical usage: \code{ahrqComorbid} is probably what you want.
#' @docType data
#' @keywords datasets
#' @format list of character vectors, each named by co-morbidity
#' @source
#' \url{http://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp}
#' @name ahrqComorbidAll
NULL

#' @title Quan adaptation of Deyo/Charlson comorbidities
#' @description Derived programmatically from the SAS code used in the original
#'   publication. According to the referenced study, this provides the best
#'   presdictor of in-patient to <30d mortality. Of note, Deyo drops the
#'   distinction between leukemia, lymphoma and non-metastatic cancer. As far as
#'   I have looked into this, in the rare cases where someone had two or three
#'   of leukemia, lymphoma and non-metastatic cancer, the Quan adaptation would
#'   give a lower Charlson score than the original scheme. The Deyo original
#'   Charlson to ICD-9-CM groups does include distinct categories for these
#'   things.
#' @docType data
#' @keywords datasets
#' @format list of character vectors, each named by co-morbidity
#' @references Quan, Hude, Vijaya Sundararajan, Patricia Halfon, Andrew Fong,
#'   Bernard Burnand, Jean-Christophe Luthi, L. Duncan Saunders, Cynthia A.
#'   Beck, Thomas E. Feasby, and William A. Ghali. "Coding Algorithms for
#'   Defining Comorbidities in ICD-9-CM and ICD-10 Administrative Data." Medical
#'   Care 43, no. 11 (November 1, 2005): 1130-39.
#'   \url{http://www.ncbi.nlm.nih.gov/pubmed/16224307}
#'   \url{http://web.archive.org/web/20110225042437/http://www.chaps.ucalgary.ca/sas}
#' @name quanDeyoComorbid
NULL

#' @title Quan adaptation of Elixhauser comorbidities
#' @description These were transcribed directly from the Quan paper referenced.
#' @docType data
#' @keywords datasets
#' @format list of character vectors, each named by co-morbidity
#' @references Quan, Hude, Vijaya Sundararajan, Patricia Halfon, Andrew Fong,
#'   Bernard Burnand, Jean-Christophe Luthi, L. Duncan Saunders, Cynthia A.
#'   Beck, Thomas E. Feasby, and William A. Ghali. "Coding Algorithms for
#'   Defining Comorbidities in ICD-9-CM and ICD-10 Administrative Data." Medical
#'   Care 43, no. 11 (November 1, 2005): 1130-39.
#'   \url{http://www.ncbi.nlm.nih.gov/pubmed/16224307}
#'   \url{http://web.archive.org/web/20110225042437/http://www.chaps.ucalgary.ca/sas}
#'
#' @name quanElixComorbid
NULL

#' @title Elixhauser comorbidities
#' @description The original mapping of Elixhauser's ICD-9-CM to 30
#'   comorbidities. According to Sharabiani et al, this mapping provides the
#'   best long-term mortality prediction. The weaknesses of this mapping are
#'   that it is based on slightly out-dated ICD-9 codes. I have not yet verified
#'   what changes to the ICD-9-CM specification between 1998 and now would
#'   impact this mapping.
#' @docType data
#' @keywords datasets
#' @format list of character vectors, each named by co-morbidity
#' @references Sharabiani, Mansour T. A., Paul Aylin, and Alex Bottle.
#'   "Systematic Review of Comorbidity Indices for Administrative Data." Medical
#'   Care December 2012 50, no. 12 (2012): 1109-18.
#'   doi:10.1097/MLR.0b013e31825f64d0.
#'   \url{http://www.ncbi.nlm.nih.gov/pubmed/22929993}
#'
#'   Elixhauser, Anne, Claudia Steiner, D. Robert Harris, and Rosanna M. Coffey.
#'   "Comorbidity Measures for Use with Administrative Data." Medical Care
#'   January 1998 36, no. 1 (1998): 8-27.
#' @name elixComorbid
NULL

#' @title Comorbidity names
#' @description These lists provide correctly sorted names of the comorbidities
#'   and their particular permutations in both full and abbreviated forms.
#'
#'   In the Elixhauser derived mappings, uncomplicated and complicated
#'   hypertension are listed separately, but are always combined in the final
#'   analyses. Uncomplicated and complicated hypertension are list separately
#'   and as "Hypertension, combined." Abbrev suffix indicates a very short
#'   space-free description. Quan's version of Elixhauser is identical. AHRQ's
#'   update drops the arrythmia field. The naming convention is a root, e.g.
#'   \code{elixComorbid}, with neither/either/both suffixes \code{Htn} and
#'   \code{Abbrev}. The Charlson derived mappings do not include hypertension.
#'   Abbreviated comorbidity names are helpful for interactive work, whereas the
#'   full names might be prefered for plotting.
#' @format list, with character/numeric code. 'Hypertension, uncomplicated' and
#'   'Hypertension, complicated' are labelled '6a' and '6b'. Diabetes, cancer,
#'   and metastasis are counted independently, as in the original paper, giving
#'   the original 30 groups. "01" to "30"
#' @name elixComorbidNames
#' @aliases elixComorbidNamesAbbrev elixComorbidNamesHtn
#'   elixComorbidNamesHtnAbbrev quanElixComorbidNames
#'   quanElixComorbidNamesAbbrev quanElixComorbidNamesHtn
#'   quanElixComorbidNamesHtnAbbrev ahrqComorbidNames ahrqComorbidNamesAbbrev
#'   ahrqComorbidNamesHtn ahrqComorbidNamesHtnAbbrev charlsonComorbidNames
#'   charlsonComorbidNamesAbbrev
#' @keywords datasets
#' @docType data
NULL

#' @title list of annual versions of billable leaf nodes of ICD-9-CM
#' @name icd9Billable
#' @description These are derived from the CMS published updates, with versions
#'   23 to 32 currently available going back to 2004/5. The source files back to
#'   version 27 have short and long descriptions. The short descriptions are in
#'   ASCII with no special characters, whereas the long descriptions contain
#'   accented characters which seem to be interpretable as unicode, latin-1 or
#'   cp1252. This all done during package creation, but can be repeated by
#'   package users, including pulling the data from the web pages directly.
#'   Despite my best efforts, current locale can give different results, but
#'   this packaged data is correct, with some UTF-8 encoded strings.
#' @docType data
#' @keywords datasets
#' @format list of data frames. Each list item is named by the version as a
#'   string, e.g. "32". The constituent data frames have columns \code{icd9},
#'   \code{shortDesc}, and \code{longDesc}.
#' @source \url{http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html}
NULL

# we don't ever use magrittr in 'live' package use, just when it is using its
# own functions for testing and generating its own data: in those cases magrittr
# will be available, but we don't want CRAN check problems, so:
# utils::globalVariables(c("%<>%"))

#' @title de-identified data from public Vermont source for 2013
#' @name vermont_dx
#' @details Conditions of Release Release of public use data is subject to the
#'   following conditions, which the requestor agrees to upon accepting copies
#'   of the data:
#'
#'   1. The data may not be used in any manner that attempts to or does
#'   identify, directly or indirectly, any individual patient or physician.
#'
#'   2. The requestor agrees to incorporate the following, or a substantially
#'   similar, disclaimer in all reports or publications that include public use
#'   data: "Hospital discharge data for use in this study were supplied by the
#'   Vermont Association of Hospitals and Health Systems-Network Services
#'   Organization (VAHHS-NSO) and the Vermont Department of Banking, Insurance,
#'   Securities and Health Care Administration (BISHCA). All analyses,
#'   interpretations or conclusions based on these data are solely that of [the
#'   requestor]. VAHHS-NSO and BISHCA disclaim responsibility for any such
#'   analyses, interpretations or conclusions. In addition, as the data have
#'   been edited and processed by VAHHS-NSO, BISHCA assumes no responsibility
#'   for errors in the data due to coding or processing"
#' @source
#' \url{http://healthvermont.gov/research/hospital-utilization/RECENT_PU_FILES.aspx}
#'
#' @format CSV original, minimally processed into R data frame.
#' @keywords datasets
#' @author Vermont Division of Health Care Administration
#' @docType data
.vermont <- function() {

  file_path = unzip_to_data_raw(
    url = "http://healthvermont.gov/research/hospital-utilization/VTINP13.zip",
    file_name = "VTINP13.TXT")
  vermont_dx <- utils::read.csv(file_path,
                                stringsAsFactors = FALSE,
                                strip.white = TRUE,
                                nrows = 1001)[, c(74, 4, 6, 7, 11, 13:32)]
  age_group <- vermont_dx$intage
  attr(age_group, "class") <- "factor"
  attr(age_group, "levels") <- c("Under 1", "1-17", "18-24",
                                 "25-29", "30-34", "35-39",
                                 "40-44", "45-49", "50-54",
                                 "55-59", "60-64", "65-69",
                                 "70-74", "75 and over",
                                 "Unknown")
  sex <- vermont_dx$sex
  attr(sex, "class") <- "factor"
  attr(sex, "levels") <- c("male", "female", "unknown")
  vermont_dx$intage <- age_group
  vermont_dx$sex <- sex
  vermont_dx$dstat <- vermont_dx$dstat == 8 # death (other codes are for various discharge statuses)
  names(vermont_dx)[c(1:5)] <- c("visit_id", "age_group", "sex", "death", "DRG")
  class(vermont_dx) <- c("icd9cm", "icd9", "icd_short_code", "icd_wide_data", "data.frame")
  vermont_dx %<>% head(1000)

  class(vermont_dx) <- c("icd9cm", "icd9", "icd_wide", "data.frame")

  save_in_data_dir(vermont_dx)
  invisible(vermont_dx)
}

#' @title United States Transuranium & Uranium Registries
#' @name uranium_pathology
#' @source \url{http://www.ustur.wsu.edu/database/}
#' \url{http://www.ustur.wsu.edu/Case_Studies/Pathology/mdb/Pathology_Office2007.zip}
#' @docType data
#' @keywords datasets
.uranium_pathology <- function() {

  file_path <- unzip_to_data_raw(
    url = "http://www.ustur.wsu.edu/Case_Studies/Pathology/mdb/Pathology_Office2007.zip",
    file_name = "Pathology_Office2007.accdb")

  # odbcConnectAccess2007 is only in the Windows version of RODBC
  channel <- RODBC::odbcConnectAccess2007(file_path)
  uranium_pathology <- RODBC::sqlFetch(channel, "qry_ICD-10")

  uranium_pathology <- uranium_pathology[, c("Case_No", "ICD-10_code")]
  names(uranium_pathology) <- c("case", "icd10")

  uranium_pathology <- uranium_pathology[order(uranium_pathology["case"]), ]

  row.names(uranium_pathology) <- 1:nrow(uranium_pathology)

  class(uranium_pathology) <- c("icd10who", "icd10", "icd_long_data", "icd_decimal_code", "data.frame")

  save_in_data_dir(uranium_pathology)
  invisible(uranium_pathology)
}
