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

#' @details \describe{ \item{Comorbidities}{ \code{\link{icd_comorbid}}
#'   determines comorbidities for a set of patients with one or more ICD-9
#'   codes each. \code{\link{icd9Charlson}} calculates Charlson score
#'   (Comorbidity Index).
#'
#'   \itemize{
#'
#'   \item AHRQ comorbidity mapping is provided, and a function to read the raw
#'   SAS code from AHRQ into R data structures. The data is available by
#'   lazy-loading in \code{\link{icd9_map_ahrq}}. AHRQ releases new mappings
#'   annually.
#'
#'   \item Quan revised both Deyo/Charlson and Elixhauser ICD-9 to comorbidity
#'   mappings. These are presented as: \code{link{icd9_map_quan_deyo}} (which is
#'   also derived from the original SAS code used in his publication, referenced
#'   in the data documentation), \code{link{icd10_map_quan_deyo}},
#'   \code{\link{icd9_map_quan_elix}}, and \code{\link{icd10_map_quan_elix}}
#'   which was transcribed directly from the same paper.
#'
#'   \item The original Elixhauser mapping is provided, with codes transcribed
#'   from the original publication. See \code{\link{icd9_map_elix}}.
#'
#'   } }
#'
#'   \item{Validation}{\code{\link{icd_is_valid}} checks whether ICD-9 codes are
#'   syntactically valid (although not necessarily genuine ICD-9 diagnoses). In
#'   contrast, \code{\link{icd_is_defined}} checks whether ICD-9 codes
#'   correspond to diagnoses in the current ICD-9-CM definition from CMS.}
#'
#'   \item{Conversion}{ There are many functions to convert ICD-9 codes or their
#'   components between different formats and structures. The most commonly used
#'   are:  \code{\link{icd9DecimalToShort}}, \code{\link{icd9ShortToDecimal}} to
#'   convert, e.g., 002.3 to 0023 and back again. See \link{convert} for other
#'   options.}
#'
#'   \item{Manipulation}{You can find children of a higher-level ICD-9 code with
#'   \code{\link{icd_children}} and find a common parent to a set of children
#'   (or arbitrary list of ICD-9 codes) with \code{\link{icd_condense}}.
#'   \code{\link{icd_sort}} sorts in hierarchical, then numerical order, so
#'   100.0 comes before 100.00, for example. \code{\link{icd_wide_to_long}} and
#'   \code{\link{icd9LongToWide}} convert the two most common data structures
#'   containing patient disease data. This is more sophisticated and tailored to
#'   the problem than base reshaping or extension packages, although these could
#'   no doubt be used.}
#'
#'   \item{Explanation, or decoding}{Use \code{\link{icd_explain}} to convert a
#'   list of codes into human-readable descriptions. This function can
#'   optionally reduce the codes to a their top-level groups if all the child
#'   members of a group are present. \code{\link{icd_diff_comorbid}} allows
#'   summary of the differences between comorbidity mappings, e.g. to find what
#'   has changed from year-to-year or between revisions by different authors.
#'   \code{\link{icd9cm_hierarchy}} is a \code{data.frame} containing the full
#'   ICD-9 classification for each diagnosis. \code{\link{icd9_chapters}}
#'   contains definitions of chapters, sub-chapters and three-digit groups.}
#'
#'   }
"_PACKAGE"
#' @docType package
#' @name icd-package
#' @aliases package-icd9 icd9-package icd10-package package-icd10
#' @author Jack O. Wasey \email{jack@@jackwasey.com}
#' @keywords misc utilities
#' @references
#' \url{http://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp}
#' @concept ICD-9 ICD-10 comorbidity comorbidities
#' @useDynLib icd
#' @import Rcpp checkmate stringr
#' @importFrom magrittr "%<>%" set_names extract2
NULL

# stringr imports and re-exports %>% so I don't get it from magrittr (otherwise
# there is a check warning)
