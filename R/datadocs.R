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

#' @title Quan adaptation of Deyo comorbidities
#' @description Derived programmatically from the SAS code used in the original 
#'   publication. According to the referenced study, this provides the best
#'   presdictor of in-patient to <30d mortality.
#' @docType data
#' @keywords datasets
#' @format list of character vectors, each named by co-morbidity
#' @references Quan, Hude, Vijaya Sundararajan, Patricia Halfon, Andrew Fong, 
#'   Bernard Burnand, Jean-Christophe Luthi, L. Duncan Saunders, Cynthia A. 
#'   Beck, Thomas E. Feasby, and William A. Ghali. “Coding Algorithms for 
#'   Defining Comorbidities in ICD-9-CM and ICD-10 Administrative Data.” Medical
#'   Care 43, no. 11 (November 1, 2005): 1130–39. 
#'   \url{http://www.ncbi.nlm.nih.gov/pubmed/16224307}
#' @name quanComorbid
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
#'   “Systematic Review of Comorbidity Indices for Administrative Data.” Medical
#'   Care December 2012 50, no. 12 (2012): 1109–18. 
#'   doi:10.1097/MLR.0b013e31825f64d0. 
#'   \url{http://www.ncbi.nlm.nih.gov/pubmed/22929993}
#'   
#'   Elixhauser, Anne, Claudia Steiner, D. Robert Harris, and Rosanna M. Coffey.
#'   “Comorbidity Measures for Use with Administrative Data.” Medical Care
#'   January 1998 36, no. 1 (1998): 8–27.
#' @name elixhauserComorbid
NULL
