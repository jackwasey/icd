#' @title ICD-9-CM chapters
#' @name icd9CmChapters
#' @description these are the top level chapter groupings. Not huge clinical signicance, but may be helpful in summarizing codes, grouping for human-readable output, and possibly as an input signal in case matching.
#' \itemize{
#' \item 001-139 Infectious And Parasitic Diseases
#' \item 140-239 Neoplasms
#' \item 240-279 Endocrine, Nutritional And Metabolic Diseases, And Immunity Disorders
#' \item 280-289 Diseases Of The Blood And Blood-Forming Organs
#' \item 290-319 Mental Disorders
#' \item 320-389 Diseases Of The Nervous System And Sense Organs
#' \item 390-459 Diseases Of The Circulatory System
#' \item 460-519 Diseases Of The Respiratory System
#' \item 520-579 Diseases Of The Digestive System
#' \item 580-629 Diseases Of The Genitourinary System
#' \item 630-679 Complications Of Pregnancy, Childbirth, And The Puerperium
#' \item 680-709 Diseases Of The Skin And Subcutaneous Tissue
#' \item 710-739 Diseases Of The Musculoskeletal System And Connective Tissue
#' \item 740-759 Congenital Anomalies
#' \item 760-779 Certain Conditions Originating In The Perinatal Period
#' \item 780-799 Symptoms, Signs, And Ill-Defined Conditions
#' \item 800-999 Injury And Poisoning
#' \item V01-V91 Supplementary Classification Of Factors Influencing Health Status And Contact With Health Services
#' \item E000-E999 Supplementary Classification Of External Causes Of Injury And Poisoning
#' }
#' @keywords datasets list
#' @docType data
#' @format list with chapter names stored in list names, each with two element named character vector with start and end codes.
NULL

#' @title ICD9-CM diagnosis code lookup
#' @description short-form ICD-9 codes with short and long descriptions. This
#'   data is limited to the most detailed codes for each condition, and does not
#'   include the higher level classification. E.g. 053 herpes is not included in
#'   these data, but all the specific types are: 0530 0531 0531[0-4] 0532 0537
#'   0538 and 0539.
#'
#'   From
#'   \url{http://wonder.cdc.gov/wonder/sci_data/codes/icd9/type_txt/icd9cm.asp}
#'   "Three-digit rubrics are not added to the main body of the classification:"
#'   this is the source of the above problem.
#' @docType data
#' @keywords datasets
#' @format data frame
#' @source
#' \url{http://wonder.cdc.gov/wonder/sci_data/codes/icd9/type_txt/icd9cm.asp}
#' @source Rich text descriptions here:
#'   \url{http://www.cdc.gov/nchs/icd/icd9cm.htm}
#' @source This is the more easily machine-readable source which is used for
#'   \code{icd9CmDesc}
#'   \url{http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html}
#' @source \url{http://wonder.cdc.gov/wonder/help/icd.html}
#' @source \url{http://wonder.cdc.gov/wonder/sci_data/codes/icd9/type_txt/icd9abb.asp}
#' @source \url{http://wonder.cdc.gov/wonder/sci_data/codes/icd9/type_txt/icd9cm.asp}
#' @source \url{http://wonder.cdc.gov/wonder/sci_data/codes/icd9/type_txt/icdcm.asp}
#' @source \url{http://wonder.cdc.gov/wonder/sci_data/codes/icd9/type_txt/icd9abb.asp}
#' @name icd9CmDesc
NULL

#' @title AHRQ comorbidities
#' @description This mapping of comorbidities to ICD-9 codes is dervied directly
#'   from SAS code provided by AHRQ, and translated into this R data structure.
#'   This is a revision of the Elixhauser system, notably exluding cradiac
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
#'   not rolled into their parent categories.
#' @docType data
#' @keywords datasets
#' @format list of character vectors, each named by co-morbidity
#' @source
#'   \url{http://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp}
#' @name ahrqComorbidAll
NULL

#' @title Quan adaptation of Deyo/Charlson comorbidities
#' @description Derived programmatically from the SAS code used in the original
#'   publication. According to the referenced study, this provides the best
#'   presdictor of in-patient to <30d mortality.
#' @docType data
#' @keywords datasets
#' @format list of character vectors, each named by co-morbidity
#' @references Quan, Hude, Vijaya Sundararajan, Patricia Halfon, Andrew Fong,
#'   Bernard Burnand, Jean-Christophe Luthi, L. Duncan Saunders, Cynthia A.
#'   Beck, Thomas E. Feasby, and William A. Ghali. "Coding Algorithms for
#'   Defining Comorbidities in ICD-9-CM and ICD-10 Administrative Data." Medical
#'   Care 43, no. 11 (November 1, 2005): 1130-39.
#'   \url{http://www.ncbi.nlm.nih.gov/pubmed/16224307}
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
#' @name quanElixhauserComorbid
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
#' @name elixhauserComorbid
NULL
