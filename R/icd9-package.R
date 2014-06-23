#' @title tools for working with ICD-9 codes, and finding comorbidities
#' @description \code{icd9} provides functions to validate and compare ICD-9 
#'   codes, and also generate co-morbidities based on ICD-9 to comorbidity 
#'   mappings (typically many:one).
#'   
#'   * \code{\link{icd9ValidDecimal}}, \code{\link{icd9ValidShort}} and related
#'   functions (\code{\link{icd9ValidDecimalN}}, \code{\link{icd9ValidDecimalV}},
#'   \code{\link{icd9ValidDecimalE}}, \code{\link{icd9ValidShortN}},
#'   \code{\link{icd9ValidShortV}}), \code{\link{icd9ValidShortE}}) check 
#'   whether given ICD-9 codes are syntactically valid (although not necessarily
#'   genuine ICD-9 codes) * \code{\link{icd9Comorbidities}} determines 
#'   co-morbidities for a set of patients with one or more ICD-9 codes each.
#'   
#'   * AHRQ comorbidity mapping is provided, and a function to read the raw SAS 
#'   code from AHRQ into R data structures. The pre-processed data is available 
#'   by lazy-loading in \code{\link{ahrqComorbid}}. AHRQ releases new mappings 
#'   annually.
#'   
#'   * \code{\link{parseAhrqSas}} interprets an SAS format file, returning R 
#'   data, intended currently only for the FORMAT code provided by AHRQ for 
#'   comorbidities.
#'   

#' @docType package
#' @name icd9-package
#' @aliases icd9 package-icd9
## @author Jack O. Wasey \email{jack@jackwasey.com}
#' @keywords misc utilities
#' @references \url{http://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp}
#' @seealso rClinicalCodes
#' @concept icd9 ICD-9 comorbidity comorbidities
NULL
