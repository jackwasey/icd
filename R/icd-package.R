#' @details \describe{ \item{Comorbidities}{ \code{\link{comorbid}} determines
#'   comorbidities for a set of patients with one or more ICD codes each. All
#'   the comorbidity functions guess which columns are the identifiers and which
#'   are ICD code fields. You may also specify these. Most 'long' or 'wide' data
#'   can simply be passed directly, e.g.: \code{comorbid(patient_data_frame)}.
#'
#'   \itemize{
#'
#'   \item The AHRQ comorbidity mappings from ICD-9 and ICD-10 are provided as
#'   \code{\link{icd9_map_ahrq}} and \code{\link{icd10_map_ahrq}}. The easiest
#'   way to use them is by calling the function \code{\link{comorbid_ahrq}}
#'   directly with your \code{\link{icd_long_data}} format patient data.
#'
#'   \item Quan revised both Deyo/Charlson and Elixhauser ICD-9 and ICD-10 to
#'   comorbidity mappings. These are presented as:
#'   \code{\link{icd9_map_quan_deyo}}, \code{\link{icd10_map_quan_deyo}},
#'   \code{\link{icd9_map_quan_elix}}, and \code{\link{icd10_map_quan_elix}}.
#'   Like the AHRQ mappings, these are all carefully extracted from the original
#'   publications or source code. These mappings can be used on patient data by
#'   calling \code{\link{comorbid_quan_deyo}} and
#'   \code{\link{comorbid_quan_elix}}
#'
#'   \item There is no canonical Charlson ICD-9 or ICD-10 mapping, so
#'   \code{\link{comorbid_charlson}} uses the thoroughly researched and widely
#'   cited  \code{\link{comorbid_quan_deyo}} method by default.
#'
#'   \item The original Elixhauser mappings are provided by the lists
#'   \code{\link{icd9_map_elix}} and \code{\link{icd10_map_elix}}, and
#'   Elixhauser comorbidities can be calculated directly from patient data using
#'   \code{\link{comorbid_elix}}.
#'
#'   \item The US Center for Medicare and Medicare services (CMS) publishes
#'   Hierarchical Condition Codes (HCC) which are essentially comorbidity maps
#'   with very many comorbidities, complicated by a single- or multi-level
#'   system. These categories can be computed using \code{\link{comorbid_hcc}}.
#'
#'   \item AHRQ also publishes Clinical Classification Software (CCS) which
#'   provides another set of disease groups, and this SAS code is implemented in
#'   'icd' by \code{\link{comorbid_ccs}}
#'
#'   } }
#'
#'   \item{Risk scoring systems}{
#'
#'   \code{\link{charlson}} calculates Charlson scores (Charlson Comorbidity
#'   Indices) directly from your patient data. If you already calculated the
#'   Charlson comorbidities, it is more efficient to use
#'   \code{\link{charlson_from_comorbid}}. Similarly, \code{\link{van_walraven}}
#'   calculates Van Walraven scores (based on the Elixhauser comorbidities,
#'   instead of Charlson), and \code{\link{van_walraven_from_comorbid}} if you
#'   already calculated Elixhauser comorbidities}
#'
#'   \item{Validation}{
#'
#'   \code{\link{is_valid}} checks whether ICD codes are syntactically valid
#'   (although not necessarily genuine ICD diagnoses). In contrast,
#'   \code{\link{is_defined}} checks whether ICD-9 codes correspond to defined
#'   diagnoses or diagnostic groups in the hierarchy of ICD codes.
#'   \code{link{is_leaf}} (and for the US, \code{link{is_billable}}) determine
#'   whether given codes are leaves in the hierarchy, or not. \pkg{icd} may need
#'   to download data due to package size or copyright restrictions on
#'   redistributing data, and needs a cache directory and your permission to do
#'   this. Use \code{setup_icd_data()} to do this, or \code{download_icd_data()}
#'   to download everything at once, which will take a few minutes on a
#'   broadband connection.
#'
#'   Validation depends on the class of code, and is different if the code is
#'   from France, Belgium, the USA, or the World Health Organization (WHO). Use
#'   the functions \code{\link{as.icd10who}}, \code{\link{as.icd10fr}},
#'   \code{\link{as.icd10be}}, and \code{\link{as.icd10cm}} to set the class of
#'   a set of ICD codes. This doesn't affect comorbidity calculations, but will
#'   change the result of the above validation functions, and }
#'
#'   \item{Conversion}{
#'
#'   There are many functions to convert ICD-9 codes or their components between
#'   different formats and structures. The most commonly used are:
#'   \code{\link{decimal_to_short}}, \code{\link{short_to_decimal}} to convert,
#'   e.g., 002.3 to 0023 and back again. See \link{convert} for other options.
#'   Conversion between ICD-9, ICD-10, and ICD-11 codes is not yet supported,
#'   but is the subject of an upcoming US National Institutes of Health (NIH)
#'   hackathon.}
#'
#'   \item{Manipulation}{
#'
#'   You can find children of a higher-level ICD-9 code with
#'   \code{\link{children}} and find a common parent to a set of children (or
#'   arbitrary list of ICD-9 codes) with \code{\link{condense}}.
#'   \code{\link{sort}} (\code{\link{sort.icd9}}, \code{\link{sort.icd10cm}},
#'   etc.) sorts in hierarchical, then numerical order, so '100.0' comes before
#'   '100.00', for example.
#'
#'   The comorbidity functions in \pkg{icd} accept 'wide' or 'long' format data,
#'   but you may wish to use \code{\link{wide_to_long}} and
#'   \code{\link{long_to_wide}}, which convert between these two most common
#'   data structures containing patient disease data. This is more sophisticated
#'   and tailored to the problem than base reshaping or packages like
#'   \CRANpkg{dplyr}, although these could no doubt be used.} These functions
#'   use base \R functions to avoid dependencies, so are not very fast for very
#'   big datasets.
#'
#'   \item{Explanation and decoding}{
#'
#'   Use \code{\link{explain_code}} to convert a set of ICD codes into
#'   human-readable descriptions. See above for discussion on WHO, French,
#'   Belgian and US ICD code classes. This function can can also reduce the
#'   codes to their top-level groups if all the child members of a group are
#'   present. \code{\link{diff_comorbid}} allows summary of the differences
#'   between comorbidity mappings, e.g. between revisions by different authors.
#'   } }
#' @docType package
#' @name icd-package
#' @aliases icd icd-package package-icd package-icd9 icd9-package icd10-package
#'   package-icd10
#' @author Jack O. Wasey \email{jack@@jackwasey.com}
#' @keywords misc utilities internal
#' @concept comorbidity comorbidities comorbid ICD-9 ICD-10 ICD
#' @useDynLib icd, .registration=TRUE
#' @importFrom Rcpp cppFunction
#' @importFrom utils head tail read.fwf
#' @importFrom stats setNames reshape
#' @importFrom graphics barplot
#' @importFrom methods is
"_PACKAGE"
