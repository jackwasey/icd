#' @title ICD9-CM diagnosis code lookup
#' @description short-form ICD-9 codes with short and long descriptions
#' @docType data
#' @keywords datasets
#' @format data frame
#' @source \url{http://www.cdc.gov/nchs/icd/icd9cm.htm}
#' @name icd9CmDesc
NULL

#' @title AHRQ comorbidities
#' @description This mapping of comorbidities to ICD-9 codes is dervied directly
#'   from SAS code provided by AHRQ, and translated into this R data structure.
#' @docType data
#' @keywords datasets
#' @format list of character vectors
#' @source \url{http://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp}
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

#' @title Quan adaptation of Deyo comorbidities
#' @description Derived programmatically from the SAS code used in the original
#'   publication.
#'   @docType data
#'   @keywords datasets
#'   @format list of character vectors, each named by co-morbidity
#' @references Quan, Hude, Vijaya Sundararajan, Patricia Halfon, Andrew Fong, 
#'   Bernard Burnand, Jean-Christophe Luthi, L. Duncan Saunders, Cynthia A. 
#'   Beck, Thomas E. Feasby, and William A. Ghali. “Coding Algorithms for 
#'   Defining Comorbidities in ICD-9-CM and ICD-10 Administrative Data.” Medical
#'   Care 43, no. 11 (November 1, 2005): 1130–39. 
#'   \url{http://www.ncbi.nlm.nih.gov/pubmed/16224307}
#'   @name quanComorbidity
NULL
