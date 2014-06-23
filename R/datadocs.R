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
#'   from SAS code provided by AHRQ, and translated into this R data structure. Beyond ahrqComorbid, this includes all the HTN, CHF and renal subgroups, not rolled into their parent categories.
#' @docType data
#' @keywords datasets
#' @format list of character vectors
#' @source \url{http://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp}
#' @name ahrqComorbidAll
NULL
