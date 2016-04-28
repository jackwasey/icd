# Copyright (C) 2014 - 2016  Jack O. Wasey
#
# This file is part of icd.
#
# icd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd. If not, see <http:#www.gnu.org/licenses/>.

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
#' \url{http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html}
#'
#' @format list with chapter/sub-chapter or major names stored in list names,
#'   each with two element named character vector with start and end codes.
#' @name icd9_chapters
#' @aliases icd9Chapters icd9ChaptersSub icd9ChaptersMajor icd9_sub_chapters
#'   icd9_majors
NULL

#' ICD-10 chapters
#'
#' The WHO ICD-10 scheme chapters. The chapter level is the highest in the
#' hierarchy, each chapter containing sets of codes which span multiple
#' three-digit 'major' codes, and in some cases also span codes across two
#' alphabetic initial characters. E.g. Chapter I spans A00 to B99.
#' @details TODO: confirm that ICD-10-CM is identical to ICD-10 at this level
#' @source http://apps.who.int/classifications/icd10/browse/2016/en
#' @format list with chapter names stored in list names, each with two element
#'   named character vector with start and end codes.
#' @name icd10_chapters
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
#' @format list with sub-chapter or major names stored in list names, each with
#'   two element named character vector with start and end codes.
#' @name icd10_sub_chapters
NULL


#' Latest ICD-9-CM diagnosis codes, in flat \code{data.frame} format
#'
#' Short-form ICD-9 codes with short and long descriptions, and description of
#' each hierarchy level containing each code.
#'
#' \code{icd9Hierarchy} is deprecated in favor of \code{icd9cm_hierarchy}
#'
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
#' @name icd9cm_hierarchy
#' @aliases icd9Hierarchy
NULL

#' ICD-10-CM
#'
#' The public domain modified ICD-10 classification as published in the public
#' domain by the US CDC. Currently this has a slightly different structure to
#' \code{icd9cm_hierarchy} because the published data helpfully has a
#' \emph{leaf} flag indicating whether a code is a \emph{billable} leaf node, or
#' a code higher in the hierarchy which nevertheless will have a description.
#'
#' There are annual revisions to this data. Currently, the 2016 edition is
#' included.
#' @name icd10cm2016
#' @source \url{http://www.cdc.gov/nchs/icd/icd10cm.htm}
#' @docType data
#' @keywords datasets
#' @format data frame, with columns for code, leaf status (0 or 1), short and
#'   long descriptions.
NULL

#' AHRQ comorbidities
#'
#' This mapping of comorbidities to ICD-9 codes is derived directly from SAS
#' code provided by AHRQ, and translated into this R data structure. This is a
#' revision of the Elixhauser system, notably excluding cardiac arrythmia.
#' @docType data
#' @keywords datasets
#' @format list of character vectors
#' @source
#' \url{http://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp}
#' @name icd9_map_ahrq
#' @aliases ahrqComorbid ahrq icd10_map_ahrq
NULL

#' Quan adaptation of Deyo/Charlson comorbidities
#'
#' Derived automatically from the SAS code used in the original publication.
#' According to the referenced study, this provides the best predictor of
#' in-patient to <30d mortality. Of note, Deyo drops the distinction between
#' leukemia, lymphoma and non-metastatic cancer. As far as I have looked into
#' this, in the rare cases where someone had two or three of leukemia, lymphoma
#' and non-metastatic cancer, the Quan adaptation would give a lower Charlson
#' score than the original scheme. The Deyo original Charlson to ICD-9-CM groups
#' does include distinct categories for these things.
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
#' @name icd9_map_quan_deyo
#' @aliases quanDeyoComorbid icd10_map_quan_deyo
NULL

#' Quan adaptation of Elixhauser comorbidities
#'
#' These were transcribed directly from the Quan paper referenced.
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
#'
#' @name icd9_map_quan_elix
#' @aliases quanElixComorbid icd10_map_quan_elix
NULL

#' Elixhauser comorbidities
#'
#' The original mapping of Elixhauser's ICD-9-CM to 30 comorbidities. According
#' to Sharabiani, this mapping provides the best long-term mortality prediction.
#' The weaknesses of this mapping are that it is based on slightly out-dated
#' ICD-9 codes. I have not yet verified what changes to the ICD-9-CM
#' specification between 1998 and now would impact this mapping.
#' @docType data
#' @keywords datasets
#' @format list of character vectors, each named by co-morbidity
#' @references Sharabiani, Mansour T. A., Paul Aylin, and Alex Bottle.
#'   "Systematic Review of Comorbidity Indices for Administrative Data."
#' 	 Medical Care December 2012 50, no. 12 (2012): 1109-18.
#'   doi:10.1097/MLR.0b013e31825f64d0.
#'   \url{http://www.ncbi.nlm.nih.gov/pubmed/22929993}
#'
#'   Elixhauser, Anne, Claudia Steiner, D. Robert Harris, and Rosanna M. Coffey.
#'   "Comorbidity Measures for Use with Administrative Data." Medical Care
#'   January 1998 36, no. 1 (1998): 8-27.
#' @name icd9_map_elix
#' @aliases elixComorbid icd10_map_elix
NULL

#' Medicare Hierarchical Condition Categories
#' 
#' Medicare HCC model was developed to use current year diagnoses and demographics
#' predict current year healthcare expenditure. This classification has been used
#' for additional risk adjustment models. ICD codes are first assigned to numeric
#' Condition Categories (CCs). A hierarchy rule is then applied so that each patient
#' is coded for only the most severe of the Condition Categories in a group. For example,
#' if a patient has metastatic lung cancer, they will only be assigned the CC
#' for "Metastatic Cancer and Acute Leukemia", and will not be assigned the 
#' CC for "Lung and other Severe Cancers". Once the hierarchy rules are applied, the codes
#' are referred to as HCCs. This mapping can change over time. It remained the same from
#' 2007-20
#' @docType data
#' @keywords datasets
#' @format \code{dataframe} with 3 columns (\code{icd_code}, \code{cc}, and \code{year})
#' @references Pope, Gregory C., et al. 
#'   "Diagnostic cost group hierarchical condition category models for Medicare risk adjustment." 
#'   Health Economics Research, Inc. Waltham, MA (2000).
#'   \url{https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Reports/Downloads/Pope_2000_2.pdf}
#' 
#'   Risk Adjustment, Centers for Medicare and Medicaid Services
#'   \url{https://www.cms.gov/Medicare/Health-Plans/MedicareAdvtgSpecRateStats/Risk-Adjustors.html}
#' @name icd9_map_hcc
#' @aliases icd10_map_hcc

#' Comorbidity names
#'
#' These lists provide correctly sorted names of the comorbidities and their
#' particular permutations in both full and abbreviated forms.
#'
#' In the Elixhauser derived mappings, uncomplicated and complicated
#' hypertension are listed separately, but are always combined in the final
#' analyses. Uncomplicated and complicated hypertension are list separately and
#' as "Hypertension, combined." _abbrev suffix indicates a very short space-free
#' description. Quan's version of Elixhauser is identical. AHRQ updates drops
#' the arrythmia field. The naming convention is a root, e.g.
#' \code{elixComorbid}, with neither/either/both suffixes \code{_htn} and
#' \code{_abbrev}. The Charlson derived mappings do not include hypertension.
#' _abbreviated comorbidity names are helpful for interactive work, whereas the
#' full names might be preferred for plotting.
#' @format list, with character/numeric code. 'Hypertension, uncomplicated' and
#'   'Hypertension, complicated' are labelled '6a' and '6b'. Diabetes, cancer,
#'   and metastasis are counted independently, as in the original paper, giving
#'   the original 30 groups. "01" to "30"
#' @name icd_names_elix
#' @aliases icd_names_elix_abbrev icd_names_elix_htn icd_names_elix_htn_abbrev
#'   icd_names_quan_elix icd_names_quan_elix_abbrev icd_names_quan_elix_htn
#'   icd_names_quan_elix_htn_abbrev icd_names_ahrq icd_names_ahrq_abbrev
#'   icd_names_ahrq_htn icd_names_ahrq_htn_abbrev icd_names_charlson
#'   icd_names_charlson_abbrev elixComorbidNames elixComorbidNamesAbbrev
#'   elixComorbidNamesHtn elixComorbidNamesHtnAbbrev quanElixComorbidNames
#'   quanElixComorbidNamesAbbrev quanElixComorbidNamesHtn
#'   quanElixComorbidNamesHtnAbbrev ahrqComorbidNames ahrqComorbidNamesAbbrev
#'   ahrqComorbidNamesHtn ahrqComorbidNamesHtnAbbrev charlsonComorbidNames
#'   charlsonComorbidNamesAbbrev
#' @keywords datasets
#' @docType data
NULL

#' list of annual versions of billable leaf nodes of ICD-9-CM
#'
#' These are derived from the CMS published updates, with versions 23 to 32
#' currently available going back to 2004/5. The source files back to version 27
#' have short and long descriptions. The short descriptions are in ASCII with no
#' special characters, whereas the long descriptions contain accented characters
#' which seem to be interpreted as Unicode, \code{latin-1} or \code{cp1252}.
#' This all done during package creation, but can be repeated by package users,
#' including pulling the data from the web pages directly. Despite my best
#' efforts, current locale can give different results, but this packaged data is
#' correct, with some \code{UTF-8} encoded strings.
#' @docType data
#' @keywords datasets
#' @format list of data frames. Each list item is named by the version as a
#'   string, e.g. "32". The constituent data frames have columns \code{icd9},
#'   \code{shortDesc}, and \code{longDesc}.
#' @source
#' \url{http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html}
#'
#' @name icd9cm_billable
#' @aliases icd9Billable
NULL

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
#' data: "Hospital discharge data for use in this study were supplied by the
#' Vermont Association of Hospitals and Health Systems-Network Services
#' Organization (VAHHS-NSO) and the Vermont Department of Banking, Insurance,
#' Securities and Health Care Administration (BISHCA). All analyses,
#' interpretations or conclusions based on these data are solely that of [the
#' requestor]. VAHHS-NSO and BISHCA disclaim responsibility for any such
#' analyses, interpretations or conclusions. In addition, as the data have been
#' edited and processed by VAHHS-NSO, BISHCA assumes no responsibility for
#' errors in the data due to coding or processing"
#' @source
#' \url{http://healthvermont.gov/research/hospital-utilization/RECENT_PU_FILES.aspx}
#' @format CSV original, minimally processed into R data frame.
#' @keywords datasets
#' @author Vermont Division of Health Care Administration
#' @docType data
#' @name vermont_dx
NULL

#' United States Transuranium & Uranium Registries
#'
#' an ICD-10 data set (not ICD-10-CM) with mortality from the United States
#' Transuranium & Uranium Registries, published in the public domain.
#' @name uranium_pathology
#' @source \url{http://www.ustur.wsu.edu/database/}
#'   \url{http://www.ustur.wsu.edu/Case_Studies/Pathology/mdb/Pathology_Office2007.zip}
#' @docType data
#' @keywords datasets
NULL
