# Copyright (C) 2014 - 2018  Jack O. Wasey
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

utils::globalVariables(c(
  "icd9_map_ahrq", "icd9_map_elix", "icd9_map_quan_deyo", "icd9_map_quan_elix",
  "icd10_map_ahrq", "icd10_map_elix", "icd10_map_quan_deyo",
  "icd10_map_quan_elix", "icd9_map_single_ccs", "icd9_map_multi_ccs",
  "icd10_map_ccs",
  "names_ahrq", "names_ahrq_abbrev", "names_ahrq_htn", "names_ahrq_htn_abbrev",
  "names_charlson", "names_charlson_abbrev", "names_elix", "names_elix_abbrev",
  "names_elix_htn", "names_elix_htn_abbrev", "names_quan_elix",
  "names_quan_elix_abbrev", "names_quan_elix_htn", "names_quan_elix_htn_abbrev"
))

#' Present-on-admission flags
#'
#' See \link{filter_poa} for more details.
#' @keywords character
#' @examples
#'   poa_choices
#' @export
poa_choices <- c("yes", "no", "notYes", "notNo")

#' Find comorbidities from ICD-9 codes.
#'
#' This is the main function which extracts comorbidities from a set of ICD-9
#' codes. Some comorbidity schemes have rules, for example, what to do when both
#' 'hypertension' and 'hypertension with complications' are present. These rules
#' are applied by default; if the exact fields from the original mappings are
#' needed, use \code{hierarchy = FALSE}. For comorbidity counting, Charlson or
#' Van Walraven scores the default should be used to apply the rules. For more
#' about computing Hierarchical Condition Codes (HCC), see
#' \code{\link{comorbid_hcc}} For more about comorbidities following the
#' Clinical Classification Software (CCS) rules from AHRQ, see
#' \code{\link{comorbid_ccs}}.
#' @param x \code{data.frame} containing a column of patient-visit identifiers
#'   and a column of ICD codes. The \code{data.frame} should be in \sQuote{long}
#'   format, like the example \code{vermont_dx} data. If it is in \sQuote{wide}
#'   format, it must be converted to \sQuote{long} using
#'   \code{\link{wide_to_long}} before calling any comorbidity functions.
#' @param map list of the comorbidities with each list item containing a vector
#'   of decimal ICD-9 codes. This is in the form of a list, with the names of
#'   the items corresponding to the comorbidities (e.g. \sQuote{HTN}, or
#'   \sQuote{diabetes}) and the contents of each list item being a character
#'   vector of short-form (no decimal place, zero left-padded) ICD codes. There
#'   is no default: the user should use the family of functions, e.g.
#'   \code{comorbid_ahrq}, since these also name the fields correctly, apply any
#'   hierarchical rules (see \code{hierarchy} below)
#' @template visit_name
#' @template icd_name
#' @template short_code
#' @template short_map
#' @template abbrev_names
#' @template hierarchy
#' @template return_df
#' @template return_binary
#' @template dotdotdot
#' @param categorize_fun Internal. Function used for the categorization problem.
#' @param comorbid_fun Internal. Function used inside categorization.
#' @inheritParams categorize restore_id_order
#' @details The order of visits may change depending on the original sequence,
#'   and the underlying algorithm used. Usually this would be the order of the
#'   first occurrence of each visit/patient identifier, but this is not
#'   guaranteed unless `restore_id_order` is set to `TRUE`.
#' @md
#' @family comorbidity computations
#' @family comorbidities
#' @examples
#' vermont_dx[1:5, 1:10]
#' vd <- wide_to_long(vermont_dx)
#' # get first few rows and columns of Charlson comorbidities using Quan's mapping
#' comorbid_quan_deyo(vd)[1:5, 1:14]
#'
#' # get summary AHRQ (based on Elixhauser) comorbidities for ICD-10 Uranium data:
#' summary(comorbid_ahrq(uranium_pathology))
#'
#' pts <- icd_long_data(visit_name = c("2", "1", "2", "3", "3"),
#'                  icd9 = c("39891", "40110", "09322", "41514", "39891"))
#' comorbid(pts, icd9_map_ahrq, short_code = TRUE) # visit_name is now sorted
#' pts <- icd_long_data(
#'            visit_name = c("1", "2", "3", "4", "4"),
#'            icd_name = c("20084", "1742", "30410", "41514", "95893"),
#'            date = as.Date(c("2011-01-01", "2011-01-02", "2011-01-03",
#'              "2011-01-04", "2011-01-04")))
#'
#' pt_hccs <- comorbid_hcc(pts, date_name = "date")
#' head(pt_hccs)
#'
#' pts10 <- icd_long_data(
#'   visit_name = c("a", "b", "c", "d", "e"),
#'   icd_name =c("I058", NA, "T82817A", "", "I69369"),
#'   date = as.Date(
#'     c("2011-01-01", "2011-01-02", "2011-01-03", "2011-01-03", "2011-01-03")))
#'
#' icd10_comorbid(pts10, map = icd10_map_ahrq)
#' # or if library(icd) hasn't been called first:
#' icd::icd10_comorbid(pts10, map = icd::icd10_map_ahrq)
#' # or most simply:
#' icd::icd10_comorbid_ahrq(pts10)
#'
#' # specify a simple custom comorbidity map:
#' my_map <- list("malady" = c("100", "2000"),
#'                "ailment" = c("003", "040"))
#' two_pts <- data.frame(visit_id = c("v01", "v01", "v02", "v02"),
#'   icd9 = as.icd9(c("040", "000", "100", "000")),
#'   stringsAsFactors = FALSE)
#' comorbid(two_pts, map = my_map)
#' @export
comorbid <- function(x, map,
                     visit_name = NULL,
                     icd_name = NULL,
                     short_code = guess_short(x, icd_name = icd_name),
                     short_map = guess_short(map),
                     return_df = FALSE, return_binary = FALSE,
                     categorize_fun = categorize_simple,
                     ...)
  switch_ver_cmb(x, funs = list(icd9 = icd9_comorbid, icd10 = icd10_comorbid),
                 map = map, visit_name = visit_name, icd_name = icd_name,
                 short_code = short_code, return_df = return_df,
                 return_binary = return_binary, ...)

#' @describeIn comorbid ICD-10 comorbidities
#' @param icd10_comorbid_fun Internal function Default will be fast and
#'   accurate. A function which calculates comorbidities for ICD-10 codes, in
#'   which the comorbidity map only specifies parent codes, not every possible
#'   child.
#' @export
icd10_comorbid <- function(x,
                           map,
                           visit_name = NULL,
                           icd_name = NULL,
                           short_code = NULL,
                           short_map = guess_short(map),
                           return_df = FALSE, return_binary = FALSE,
                           icd10_comorbid_fun = icd10_comorbid_reduce, ...) {
  assert_data_frame(x, min.cols = 2, col.names = "unique")
  assert_list(map, min.len = 1, names = "unique")
  assert(check_string(visit_name), check_null(visit_name))
  assert(check_character(icd_name), check_null(icd_name))
  visit_name <- get_visit_name(x, visit_name)
  icd_name <- get_icd_name(x, icd_name)
  assert_string(visit_name)
  assert(check_flag(short_code), check_null(short_code))
  assert_flag(short_map)
  if (is.null(icd_name))
    icd_name <- get_icd_name(x)
  if (is.null(short_code))
    short_code <- guess_short(x[[icd_name[1]]])
  icd10_comorbid_fun(x = x, map = map, visit_name = visit_name,
                     icd_name = icd_name, short_code = short_code,
                     short_map = short_map, return_df = return_df,
                     return_binary = return_binary,
                     categorize_fun = categorize_simple,
                     ...)
}

#' ICD-10 comorbidities by reducing problem size
#'
#' Use method to reduce ICD-10 problem by initially finding only relevant codes
#' in the map, then populating map only with the exact patient ICD codes (not
#' the original map codes), before doing categorization.
#' @keywords internal
icd10_comorbid_reduce <- function(x = x, map, visit_name, icd_name, short_code,
                                  short_map, return_df,
                                  return_binary = FALSE,
                                  categorize_fun = categorize_simple, ...) {
  if (!short_code)
    x[icd_name] <- lapply(x[icd_name], decimal_to_short.icd10)
  # TODO: could  reduce list of input visits here, as we are scanning the codes
  # TODO: must we factor here?
  x[icd_name] <- lapply(x[icd_name], factor_nosort_rcpp, na.rm = FALSE)
  reduced_map <- simplify_map_lex(
    pt_codes = unlist(lapply(x[icd_name], levels)),
    map = map
    )
  categorize_fun(x = x, map = reduced_map,
                 id_name = visit_name, code_name = icd_name,
                 return_df = return_df, return_binary = return_binary, ...)
}

#' @describeIn comorbid Get comorbidities from \code{data.frame} of ICD-9
#'   codes
#' @param preclean single logical value, which, if \code{TRUE} causes ICD-9
#'   'short' code input to be padded to correct three (or four for E code)
#'   length before applying the comorbidity map. For very large data sets, e.g.
#'   ten million rows, this is much slower than the comorbidity calculation. If
#'   you know that the source ICD-9 codes are already well formed (or have
#'   already run \code{icd9_add_leading_zeroes}), then \code{preclean} can be
#'   set to \code{FALSE} to save time.
#' @export
icd9_comorbid <- function(x, map, visit_name = NULL, icd_name = NULL,
                          short_code = guess_short(x, icd_name = icd_name),
                          short_map = guess_short(map),
                          return_df = FALSE, return_binary = FALSE,
                          preclean = TRUE,
                          categorize_fun = categorize_simple,
                          comorbid_fun = comorbidMatMulWide,
                          ...) {
  assert_data_frame(x, min.cols = 2, col.names = "unique")
  assert_list(map, min.len = 1, names = "unique")
  assert(check_string(visit_name), check_null(visit_name))
  assert(check_character(icd_name), check_null(icd_name))
  visit_name <- get_visit_name(x, visit_name)
  icd_name <- get_icd_name(x, icd_name)
  stopifnot(is.character(icd_name) || is.null(icd_name))
  assert_string(visit_name)
  #assert_string(icd_name)
  assert_flag(short_code)
  assert_flag(short_map)
  # confirm class is ICD-9, and add leading zeroes if missing, and if levels
  # like 010 and 10 exists, then these get contracted by decimal_to_short,
  # making the results different if icd codes are short or not.
  if (!short_code) {
    #x[[icd_name]] <- decimal_to_short.icd9(x[[icd_name]])
    x[icd_name] <- lapply(x[icd_name], decimal_to_short.icd9)
  } else if (preclean) {
    #x[[icd_name]] <- icd9_add_leading_zeroes(x[[icd_name]], short_code = TRUE)
    x[icd_name] <- lapply(x[icd_name], icd9_add_leading_zeroes, short_code = TRUE)
  }
  if (!short_map)
    map <- lapply(map, decimal_to_short)
  categorize_fun(x = x, map = map, id_name = visit_name,
                 code_name = icd_name, return_df = return_df,
                 return_binary = return_binary, comorbid_fun = comorbid_fun,
                 ...)
}

#' @describeIn comorbid AHRQ comorbidities for ICD-9 codes
#' @details \code{data.frame}s of patient data may have columns within them
#'   which are of class \code{icd9}, \code{icd10} etc., but do not themselves
#'   have a class: therefore, the S3 mechanism for dispatch is not suitable. I
#'   may add a wrapper function which looks inside a \code{data.frame} of
#'   comorbidities, and dispatches to the appropriate function, but right now
#'   the user must call the \code{icd9_} or \code{icd10_} prefixed function
#'   directly.
#' @export
icd9_comorbid_ahrq <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd9_comorbid(x, map = icd::icd9_map_ahrq, short_map = TRUE, ...)
  apply_hier_ahrq(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @describeIn comorbid AHRQ comorbidities for ICD-10 codes
#' @export
icd10_comorbid_ahrq <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd10_comorbid(x, map = icd::icd10_map_ahrq, short_map = TRUE, ...)
  apply_hier_ahrq(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @describeIn comorbid Elixhauser comorbidities for ICD-9 codes
#' @export
icd9_comorbid_elix <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd9_comorbid(x, map = icd::icd9_map_elix, short_map = TRUE, ...)
  apply_hier_elix(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @describeIn comorbid Elixhauser comorbidities for ICD-10 codes
#' @export
icd10_comorbid_elix <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd10_comorbid(x, map = icd::icd10_map_elix, short_map = TRUE, ...)
  apply_hier_elix(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @describeIn comorbid Quan's Elixhauser comorbidities for ICD-9 codes
#' @export
icd9_comorbid_quan_elix <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd9_comorbid(x, map = icd::icd9_map_quan_elix, short_map = TRUE, ...)
  apply_hier_quan_elix(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @describeIn comorbid Quan's Elixhauser comorbidities for ICD-10 codes
#' @export
icd10_comorbid_quan_elix <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd10_comorbid(x, map = icd::icd10_map_quan_elix, short_map = TRUE, ...)
  apply_hier_quan_elix(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @describeIn comorbid Quan's Deyo (Charlson) comorbidities for ICD-9 codes
#' @export
icd9_comorbid_quan_deyo <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd9_comorbid(x, map = icd::icd9_map_quan_deyo, short_map = TRUE, ...)
  apply_hier_quan_deyo(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @describeIn comorbid Quan's Deyo (Charlson) comorbidities for ICD-10 codes
#' @export
icd10_comorbid_quan_deyo <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd10_comorbid(x, map = icd::icd10_map_quan_deyo, short_map = TRUE, ...)
  apply_hier_quan_deyo(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @describeIn comorbid Currently synonym for \code{icd9_comorbid_quan_deyo}
#' @export
icd9_comorbid_charlson <- function(...)
  icd9_comorbid_quan_deyo(...)

#' @describeIn comorbid Currently synonym for \code{icd10_comorbid_quan_deyo}
#' @export
icd10_comorbid_charlson <- function(...)
  icd10_comorbid_quan_deyo(...)

#' @describeIn comorbid Use AHRQ CCS for comorbidity classification
#' @seealso \link{icd9_map_single_ccs}
#' @export
comorbid_ccs <- function(x, icd_name = get_icd_name(x), ...)
  switch_ver_cmb(x, list(icd9 = icd9_comorbid_ccs,
                         icd10 = icd10_comorbid_ccs), ...)

#' @describeIn comorbid Compute AHRQ Clinical Classifications Software (CCS)
#'   scores from ICD-9 codes
#' @template ccs-single
#' @param lvl If multiple level CCS, then level must be selected as a number
#'   between one and four.
#' @export
icd9_comorbid_ccs <- function(x, ...,
                              single = TRUE,
                              lvl = NULL,
                              map = icd::icd9_map_single_ccs,
                              short_map = TRUE) {
  assert_flag(single)
  assert_int(lvl, lower = 1, upper = 4, null.ok = TRUE, na.ok = FALSE)
  if (!single) {
    if (!is.null(lvl))
      map <- icd::icd9_map_multi_ccs[[paste0("lvl", lvl)]]
    else
      stop("If 'single' is false, then 'lvl' must be supplied as 1, 2, 3 or 4")
  }
  icd9_comorbid(x, map = map, short_map = short_map, ...)
}

#' @describeIn comorbid Compute AHRQ Clinical Classifications Software (CCS)
#'   scores from ICD-10 codes
#' @export
icd10_comorbid_ccs <- function(x, ..., single = TRUE, lvl = NULL) {
  assert_flag(single)
  assert_int(lvl, lower = 1, upper = 2, null.ok = TRUE, na.ok = FALSE)
  m <- icd::icd10_map_ccs$single
  if (!single) {
    if (!is.null(lvl))
      m <- icd::icd10_map_ccs[[paste0("lvl", lvl)]]
    else
      stop("If 'single' is false, then 'lvl' must be supplied as 1 or 2")
  }
  icd10_comorbid(x, map = m, short_map = TRUE, ...)
}

#' @describeIn comorbid AHRQ comorbidities, infers whether to use ICD-9 or
#'   ICD-10 codes
#' @export
comorbid_ahrq <- function(x, ...)
  switch_ver_cmb(x, list(icd9 = icd9_comorbid_ahrq,
                         icd10 = icd10_comorbid_ahrq), ...)

#' @describeIn comorbid Elixhauser comorbidities, infers whether to use ICD-9 or
#'   ICD-10 codes
#' @seealso Consider using \code{\link{comorbid_ahrq}} instead of
#'   \code{\link{comorbid_elix}} for more recently updated mappings based on the
#'   Elixhauser scheme.
#' @export
comorbid_elix <- function(x, ...)
  switch_ver_cmb(x, list(icd9 = icd9_comorbid_elix,
                         icd10 = icd10_comorbid_elix), ...)

#' @describeIn comorbid Quan's Elixhauser comorbidities, infers whether to use
#'   ICD-9 or ICD-10 codes
#' @export
comorbid_quan_elix <- function(x, ...)
  switch_ver_cmb(x, list(icd9 = icd9_comorbid_quan_elix,
                         icd10 = icd10_comorbid_quan_elix), ...)

#' @describeIn comorbid Quan's Deyo (Charlson) comorbidities, infers whether to
#'   use ICD-9 or ICD-10 codes
#' @export
comorbid_quan_deyo <- function(x, ...)
  switch_ver_cmb(x, list(icd9 = icd9_comorbid_quan_deyo,
                         icd10 = icd10_comorbid_quan_deyo), ...)

#' @describeIn comorbid Calculate comorbidities using Charlson categories
#'   according to Quan/Deyo ICD categories. Synonymous with
#'   \code{link{comorbid_quan_deyo}} in this release.
#' @export
comorbid_charlson <- function(...)
  comorbid_quan_deyo(...)

#' Apply hierarchy and choose naming for each comorbidity map
#'
#' Re-used by ICD-9 and ICD-10 versions which have the same rules.
#' @param x matrix or data.frame of comorbidities
#' @details For Deyo's Charlson comorbidities, strictly speaking, there is no
#'   dropping of more e.g. uncomplicated \code{DM} if complicated \code{DM}
#'   exists, however, this is probably useful, in general and is essential when
#'   calculating the Charlson score.
#' @template abbrev_names
#' @template hierarchy
#' @name apply_hier
#' @keywords internal manip
apply_hier_elix <- function(x, abbrev_names = TRUE, hierarchy = TRUE) {
  if (hierarchy) {
    x[x[, which(colnames(x) == "Mets")] > 0, "Tumor"] <- FALSE
    x[x[, "DMcx"] > 0, "DM"] <- FALSE
    x[, "HTN"] <- (x[, "HTN"] + x[, "HTNcx"]) > 0

    # drop HTNcx without converting to vector if matrix only has one row
    x <- x[, -which(colnames(x) == "HTNcx"), drop = FALSE]
    colnames(x)[cr(x)] <- if (abbrev_names)
      names_elix_abbrev
    else
      names_elix
  } else {
    colnames(x)[cr(x)] <- if (abbrev_names)
      names_elix_htn_abbrev
    else
      names_elix_htn
  }
  x
}

#' @rdname apply_hier
#' @keywords internal manip
apply_hier_quan_elix <- function(cbd, abbrev_names = TRUE, hierarchy = TRUE) {
  if (hierarchy) {
    cbd[cbd[, "Mets"] > 0, "Tumor"] <- FALSE
    cbd[cbd[, "DMcx"] > 0, "DM"] <- FALSE
    # combine HTN
    cbd[, "HTN"] <- (cbd[, "HTN"] + cbd[, "HTNcx"]) > 0
    # drop HTNcx without converting to vector if matrix only has one row
    cbd <- cbd[, -which(colnames(cbd) == "HTNcx"), drop = FALSE]

    # if we didn't apply the hierarchy, we have to use the naming scheme with
    # HTN separated out:

    # assume that the comorbidities are the last 31 fields. At present, the
    # comorbid function doesn't attempt to aggregate fields it doesn't know
    # about, e.g. POA, or anything else the user provides in the data frame, so
    # these are just dropped, leaving the fields for visit_name and all the
    # comorbidities:
    colnames(cbd)[cr(cbd)] <- if (abbrev_names)
      names_quan_elix_abbrev
    else
      names_quan_elix
  } else {
    colnames(cbd)[cr(cbd)] <- if (abbrev_names)
      names_quan_elix_htn_abbrev
    else
      names_quan_elix_htn
  }
  cbd
}

#' @rdname apply_hier
#' @keywords internal manip
apply_hier_quan_deyo <- function(cbd, abbrev_names = TRUE, hierarchy = TRUE) {
  if (hierarchy) {
    # Use >0 rather than logical - apparently faster, and future proof against
    # change to binary from logical values in the matirx.
    cbd[cbd[, "Mets"] > 0, "Cancer"] <- FALSE
    cbd[cbd[, "DMcx"] > 0, "DM"] <- FALSE
    cbd[cbd[, "LiverSevere"] > 0, "LiverMild"] <- FALSE
  }
  colnames(cbd)[cr(cbd)] <- if (abbrev_names)
    names_charlson_abbrev
  else
    names_charlson

  cbd
}

#' @rdname apply_hier
#' @keywords internal manip
apply_hier_ahrq <- function(cbd, abbrev_names = TRUE, hierarchy = TRUE) {
  stopifnot(ncol(cbd) == 30 + is.data.frame(cbd))
  if (hierarchy) {
    # Use >0 rather than logical - apparently faster, and future proof against
    # change to binary from logical values in the matirx.
    cbd[cbd[, "Mets"] > 0, "Tumor"] <- FALSE
    cbd[cbd[, "DMcx"] > 0, "DM"] <- FALSE
    cbd[, "HTN"] <- (cbd[, "HTN"] + cbd[, "HTNcx"]) > 0
    # according to https://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp
    # diabetes with complications is NOT foldeded into one new category.

    # drop HTNcx without converting to vector if matrix only has one row
    cbd <- cbd[, -which(colnames(cbd) == "HTNcx"), drop = FALSE]
    colnames(cbd)[cr(cbd)] <- if (abbrev_names)
      names_ahrq_abbrev
    else
      names_ahrq
  } else {
    colnames(cbd)[cr(cbd)] <- if (abbrev_names)
      names_ahrq_htn_abbrev
    else
      names_ahrq_htn
  }
  cbd
}

#' sequence columns of comorbidities
#'
#' Get sequence of column indices of comorbidity data frame, which differ
#' between tidy data frames and clean, row-named matrices
#' @param x matrix or data.frame of comorbidities
#' @keywords internal
cr <- function(x)
  seq(from = 1 + is.data.frame(x), to = ncol(x))
