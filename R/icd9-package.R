#' @title tools for working with ICD-9 codes, and finding comorbidities
#' @description \code{icd9} package allows accurate manipulation, validation and
#'   comparison of ICD-9-CM (clinical modification) codes. Although ICD-9 codes
#'   appear numeric, this is not the case, since trailing zeroes, and a mixture
#'   of decimal and non-decimal "short" format codes exist. Tools to map ICD-9
#'   codes to co-morbidities are also provided, with mappings from Quan,
#'   Elixhauser and AHRQ included.
#'
#'   * \code{\link{icd9Valid}}\code{\link{icd9ValidDecimal}},
#'   \code{\link{icd9ValidShort}} and related functions
#'   (\code{\link{icd9ValidDecimalN}}, \code{\link{icd9ValidDecimalV}},
#'   \code{\link{icd9ValidDecimalE}}, \code{\link{icd9ValidShortN}},
#'   \code{\link{icd9ValidShortV}}), \code{\link{icd9ValidShortE}}) check
#'   whether given ICD-9 codes are syntactically valid (although not necessarily
#'   genuine ICD-9 codes) * \code{\link{icd9Comorbid}} determines
#'   co-morbidities for a set of patients with one or more ICD-9 codes each.
#'
#'   * Functions to convert vectors (not lists) of ICD-9 codes include:
#'   \code{\link{icd9DecimalToShort}}, \code{\link{icd9ShortToDecimal}}.
#'   \code{\link{icd9ShortToParts}} and \code{\link{icd9DecimalToParts}} split a
#'   vector of ICD-9 codes into the 'major' part (before decimal point), and
#'   'minor' part (after decimal). The inverse operations are
#'   \code{\link{icd9PartsToShort}} and \code{\link{icd9PartsToDecimal}}.
#'
#'   * Several functions interpret or generate ranges of ICD-9 codes. ICD-9
#'   codes in publications are often specified in ranges, and this requires
#'   careful interpretation to avoid including overly broad intermediate codes
#'   that might appear in a simple numerical range. See the test suite for some
#'   example cases. \code{\link{icd9ChildrenDecimal}} and
#'   \code{\link{icd9ChildrenShort}} generate the full range of child ICD-9
#'   codes. The reverse operation is \code{\link{icd9CondenseToMajor}}, which
#'   takes a mass of codes and finds the smallest set of parent 'major' codes
#'   with any of the original codes, which exactly describes the original set.
#'   \code{\link{icd9CondenseToExplain}}) does the same as
#'   \code{\link{icd9CondenseToMajor}}, but only finds parents which have a
#'   human-readable name. Many top-level codes, e.g. "391" are not themselves
#'   defined, but have description for their children. \code{\link{\%i9mj\%}}
#'   creates a range of icd9 major codes, whereas \code{\link{\%i9s\%}}
#'   (\code{\link{icd9ExpandRangeShort}}) and \code{\link{\%i9d\%}}
#'   (\code{\link{icd9ExpandRangeDecimal}}) create correct ranges of short and
#'   decimal ICD-9 codes, respectively. The only limitation is that E code
#'   ranges are not implemented (not ever seen in the wild by the author).
#'
#'   * \code{\link{icd9SortShort}} sorts in heirarchical, then numerical order,
#'   so 100.0 comes before 100.00, for example.
#'
#'   * AHRQ comorbidity mapping is provided, and a function to read the raw SAS
#'   code from AHRQ into R data structures. The pre-processed data is available
#'   by lazy-loading in \code{\link{ahrqComorbid}}. AHRQ releases new mappings
#'   annually.
#'
#'   * Quan revised both Deyo/Charlson and Elixhauser ICD-9 to comorbidity
#'   mappings. These are presented as: \code{link{quanDeyoComorbid}} (which is
#'   also derived from the original SAS code used in his publication, referenced
#'   in the data documentation), and \code{\link{quanElixComorbid}} which
#'   was transcribed directly from the same paper.
#'
#'   * The original Elixhauser mapping is provided, with codes transcribed from
#'   the original publication.
#'
#'   * \code{\link{parseAhrqSas}} interprets an SAS format file, returning R
#'   data, intended currently only for the FORMAT code provided by AHRQ for
#'   comorbidities.
#'
#' @docType package
#' @name icd9-package
#' @aliases icd9 package-icd9
#' @author Jack O. Wasey \email{jack@@jackwasey.com}
#' @keywords misc utilities
#' @references \url{http://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp}
#' @seealso rClinicalCodes comorbidities
#' @concept icd9 ICD-9 comorbidity comorbidities
#' @useDynLib icd9
#' @importFrom Rcpp sourceCpp
NULL
