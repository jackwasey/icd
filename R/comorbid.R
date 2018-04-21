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
  "icd10_map_ahrq", "icd10_map_elix", "icd10_map_quan_deyo", "icd10_map_quan_elix",
  "icd9_map_single_ccs", "icd9_map_multi_ccs", "icd10_map_ccs"
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
#' codes. This is when some trivial post-processing of the comorbidity data is
#' done, e.g. renaming to human-friendly field names, and updating fields
#' according to rules. The exact fields from the original mappings can be
#' obtained using \code{hierarchy = FALSE}, but for comorbidity counting,
#' Charlson Score, etc., the rules should be applied. For more about computing
#' Hierarchical Condition Codes (HCC), see \code{\link{comorbid_hcc}}
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
#' @param visitId Deprecated. Use \code{visit_name} instead.
#' @template icd_name
#' @param icd9Field Deprecated. Use \code{icd_name} instead.
#' @template short_code
#' @template short_map
#' @template abbrev_names
#' @template hierarchy
#' @template return_df
#' @template dotdotdot
#' @details The order of visits may change depending on the original sequence,
#'   and the underlying algorithm used. Usually this would be the order of the
#'   first occurence of each visit/patient identifier.
#'
#'   The threading of the C++ can be controlled using e.g.
#'   \code{option(icd.threads = 4)}. If it is not set, the number of cores in
#'   the machine is used. 'OpenMP' environment variables also work.
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
#'
#' # specify a simple custom comorbidity map:
#' my_map <- list("malady" = c("100", "2000"),
#'                "ailment" = c("003", "040"))
#' two_pts <- data.frame(visit_id = c("v01", "v01", "v02", "v02"),
#'   icd9 = as.icd9(c("040", "000", "100", "000")),
#'   stringsAsFactors = FALSE)
#' comorbid(two_pts, map = my_map)
#' @export
comorbid <- function(x, map, ...)
  switch_ver_cmb(x, list(icd9 = icd9_comorbid, icd10 = icd10_comorbid), map = map, ...)

#' @describeIn comorbid ICD-10 comorbidities
#' @param icd10_comorbid_fun function Internal parameter, default will be fast
#'   and accurate. A function which calculates comorbidities for ICD-10 codes,
#'   in which the comorbidity map only specifies parent codes, not every
#'   possible child.
#' @export
icd10_comorbid <- function(x,
                           map,
                           visit_name = NULL,
                           icd_name = NULL,
                           short_code = NULL,
                           short_map = guess_short(map),
                           return_df = FALSE,
                           icd10_comorbid_fun = icd10_comorbid_reduce, ...) {
  assert_data_frame(x, min.cols = 2, col.names = "unique")
  assert_list(map, any.missing = FALSE, min.len = 1, unique = TRUE, names = "unique")
  assert(check_string(visit_name), check_null(visit_name))
  assert(check_string(icd_name), check_null(icd_name))
  visit_name <- get_visit_name(x, visit_name)
  icd_name <- get_icd_name(x, icd_name)
  assert_string(visit_name)
  assert(check_flag(short_code), check_null(short_code))
  assert_flag(short_map)
  if (is.null(icd_name))
    icd_name <- get_icd_name(x)
  if (is.null(short_code))
    short_code <- guess_short(x[[icd_name]])
  icd10_comorbid_fun(x = x, map = map, visit_name = visit_name, icd_name = icd_name,
                     short_code = short_code, short_map = short_map, return_df = return_df, ...)
}

#' ICD-10 comorbidities by reducing problem size
#'
#' Use method to reduce ICD-10 problem by initially finding only relevant codes
#' in the map, then populating map only with the exact patient ICD codes (not
#' the original map codes), before doing 'comorbid_common'
#' @keywords internal
icd10_comorbid_reduce <- function(x = x, map = map, visit_name = visit_name,
                                  icd_name = icd_name, short_code = short_code,
                                  short_map = short_map, return_df = return_df, ...) {
  if (!short_code)
    x[[icd_name]] <- decimal_to_short.icd10(x[[icd_name]])

  reduced_map <- simplify_map_lex(as_char_or_levels(x[[icd_name]]), map)
  comorbid_common(x = x, map = reduced_map,
                  visit_name = visit_name, icd_name = icd_name,
                  return_df = return_df, ...)
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
icd9_comorbid <- function(x,
                          map,
                          visit_name = NULL,
                          icd_name = NULL,
                          short_code = guess_short(x, icd_name = icd_name),
                          short_map = guess_short(map),
                          return_df = FALSE,
                          preclean = TRUE,
                          visitId = NULL, #nolint
                          icd9Field = NULL, #nolint
                          ...) {
  if (!missing(visitId)) { #nolint
    warning("Use visit_name instead of visit_id.")
    if (is.null(visit_name))
      visit_name <- visitId #nolint
  }
  if (!missing(icd9Field)) {
    warning("Use icd_name instead of icd9Field.")
    if (is.null(icd_name))
      icd_name <- icd9Field #nolint
  }
  assert_data_frame(x, min.cols = 2, col.names = "unique")
  assert_list(map, any.missing = FALSE, min.len = 1, unique = TRUE, names = "unique")
  assert(check_string(visit_name), check_null(visit_name))
  assert(check_string(icd_name), check_null(icd_name))
  visit_name <- get_visit_name(x, visit_name)
  icd_name <- get_icd_name(x, icd_name)
  assert_string(visit_name)
  assert_string(icd_name)
  assert_flag(short_code)
  assert_flag(short_map)

  # confirm class is ICD-9, and add leading zeroes if missing, and if levels
  # like 010 and 10 exists, then these get contracted by decimal_to_short,
  # making the results different if icd codes are short or not.

  if (!short_code)
    x[[icd_name]] <- icd9(decimal_to_short.icd9(x[[icd_name]]))
  else if (preclean)
    x[[icd_name]] <- icd9(icd9_add_leading_zeroes(x[[icd_name]], short_code = TRUE))

  if (!short_map)
    map <- lapply(map, decimal_to_short)
  comorbid_common(x = x, map = map, visit_name = visit_name,
                  icd_name = icd_name, return_df = return_df, ...)
}

#' Internal function to calculate co-morbidities.
#'
#' This common comorbidity calculation code does not depend on ICD type. There
#' is some type conversion so the map and input codes are all in 'short' format,
#' fast factor generation, then fast comorbidity assignment.
#' @param comorbid_fun function i.e. the function (not character string) to be
#'   called to do the comorbidity calculation
#' @keywords internal
comorbid_common <- function(x,
                            map,
                            visit_name = NULL,
                            icd_name,
                            return_df = FALSE,
                            comorbid_fun = icd9Comorbid_alt_MatMul,
                            ...) {
  assert_data_frame(x, min.cols = 2, col.names = "unique")
  assert_list(map, any.missing = FALSE, min.len = 1, names = "unique")
  assert(check_string(visit_name), check_null(visit_name))
  assert(check_string(icd_name), check_null(icd_name))
  visit_name <- get_visit_name(x, visit_name)
  icd_name <- get_icd_name(x, icd_name)
  assert_string(visit_name)

  stopifnot(visit_name %in% names(x))

  map <- lapply(map, as_char_no_warn)

  # we need to convert to string and group these anyway, and much easier and
  # pretty quick to do it here:
  visit_was_factor <- is.factor(x[[visit_name]])

  if (visit_was_factor)
    iv_levels <- levels(x[[visit_name]]) # maybe superfluous as we rebuild at end?

  if (nrow(x) == 0) {
    empty_mat_out <- matrix(nrow = 0,
                            ncol = length(map),
                            dimnames = list(character(0), names(map)))
    if (!return_df) return(empty_mat_out)
    if (visit_was_factor)
      row_names <- factor_nosort(character(0), levels = iv_levels)
    else
      row_names <- character(0)
    df_out <- cbind(row_names, as.data.frame(empty_mat_out), stringsAsFactors = visit_was_factor)
    names(df_out)[1] <- visit_name
    rownames(df_out) <- NULL
    return(df_out)
  }

  # may be slow for big data. `rle` might be quicker if we know that
  # patient-visit rows are always contiguous.
  uniq_visits <- unique(x[[visit_name]]) # factor or vector

  if (!is.character(x[[visit_name]]))
    x[[visit_name]] <- as_char_no_warn(x[[visit_name]])

  # start with a factor for the icd codes in x, recode (and drop superfluous)
  # icd codes in the mapping, then do very fast match on integer without need
  # for N, V or E distinction. Char to factor conversion in R is very fast.
  relevant_codes <- intersect(
    unlist(map, use.names = FALSE),
    x[[icd_name]]
  )

  # Internally, the \code{sort} is slow. This step is one of the slowest steps
  # with very large numbers of patients. #TODO SLOW
  x[[icd_name]] <- factor_nosort(x[[icd_name]], levels = relevant_codes)
  # get the visits where there is at least one code which is not in comorbidity
  # map. many rows are NA, because most are NOT in comorbidity maps:

  # but first keep track of the visits with no comorbidities in the given map
  # using internal subset for speed
  visit_not_comorbid <- unique(
    .subset2(
      .subset(x, is.na(
        .subset2(x, icd_name))), visit_name))
  # then drop the rows where the code was not in a map

  visit_not_comorbid <- unique(.subset2(x, visit_name)[is.na(.subset2(x, icd_name))])

  x <- x[!is.na(x[[icd_name]]), ]
  # now make remove rows where there was both NA and a real code:
  visit_not_comorbid <- visit_not_comorbid[visit_not_comorbid %nin% x[[visit_name]]]

  map <- lapply(map, function(y) {
    f <- factor_nosort(y, levels = relevant_codes)
    # drop map codes that were not in the input comorbidities
    f[!is.na(f)]
  })

  # We can now do pure integer matching for icd9 codes. The only string manip
  # becomes (optionally) defactoring the visit_name for the matrix row names.
  # This is now insansely quick and not a bottleneck.
  mat_comorbid <- comorbid_fun(icd9df = x, icd9Mapping = map, visitId = visit_name,
                               icd9Field = icd_name,
                               threads = getOption("icd.threads", get_omp_cores()),
                               chunk_size = getOption("icd.chunk_size", 256L),
                               omp_chunk_size = getOption("icd.omp_chunk_size", 1L)
  ) # nolint

  # replace dropped rows (which therefore have no comorbidities) TODO SLOW even
  # just creating a large number of patients without comorbidities takes a long
  # time, then also a long time to rbind them, then sort them. A lot of time is
  # actually garbage collection.
  mat_not_comorbid <- matrix(data = FALSE,
                             nrow = length(visit_not_comorbid),
                             ncol = ncol(mat_comorbid),
                             dimnames = list(visit_not_comorbid))
  mat_comb <- rbind(mat_comorbid, mat_not_comorbid)
  # now put the visits back in original order (bearing in mind that they may not
  # have started that way)
  mat_new_row_order <- match(rownames(mat_comb), uniq_visits)
  mat <- mat_comb[order(mat_new_row_order),, drop = FALSE] #nolint

  if (!return_df)
    return(mat)
  if (visit_was_factor)
    row_names <- factor_nosort(x = rownames(mat), levels = iv_levels)
  else
    row_names <- rownames(mat)
  df_out <- cbind(row_names, as.data.frame(mat),
                  stringsAsFactors = visit_was_factor,
                  row.names = NULL)
  names(df_out)[1] <- visit_name
  rownames(df_out) <- NULL
  df_out
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
  cbd <- icd9_comorbid(x, map = icd9_map_ahrq, short_map = TRUE, ...)
  apply_hier_ahrq(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @describeIn comorbid AHRQ comorbidities for ICD-10 codes
#' @export
icd10_comorbid_ahrq <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd10_comorbid(x, map = icd10_map_ahrq, short_map = TRUE, ...)
  apply_hier_ahrq(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @describeIn comorbid Elixhauser comorbidities for ICD-9 codes
#' @export
icd9_comorbid_elix <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd9_comorbid(x, map = icd9_map_elix, short_map = TRUE, ...)
  apply_hier_elix(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @describeIn comorbid Elixhauser comorbidities for ICD-10 codes
#' @export
icd10_comorbid_elix <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd10_comorbid(x, map = icd10_map_elix, short_map = TRUE, ...)
  apply_hier_elix(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @describeIn comorbid Quan's Elixhauser comorbidities for ICD-9 codes
#' @export
icd9_comorbid_quan_elix <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd9_comorbid(x, map = icd9_map_quan_elix, short_map = TRUE, ...)
  apply_hier_quan_elix(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @describeIn comorbid Quan's Elixhauser comorbidities for ICD-10 codes
#' @export
icd10_comorbid_quan_elix <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd10_comorbid(x, map = icd10_map_quan_elix, short_map = TRUE, ...)
  apply_hier_quan_elix(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @describeIn comorbid Quan's Deyo (Charlson) comorbidities for ICD-9 codes
#' @export
icd9_comorbid_quan_deyo <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd9_comorbid(x, map = icd9_map_quan_deyo, short_map = TRUE, ...)
  apply_hier_quan_deyo(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @describeIn comorbid Quan's Deyo (Charlson) comorbidities for ICD-10 codes
#' @export
icd10_comorbid_quan_deyo <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd10_comorbid(x, map = icd10_map_quan_deyo, short_map = TRUE, ...)
  apply_hier_quan_deyo(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @describeIn comorbid Use AHRQ CCS for comorbidity classification
#' @seealso \link{icd9_map_single_ccs}
#' @export
comorbid_ccs <- function(x, icd_name = get_icd_name(x), ...)
  switch_ver_cmb(x, list(icd9 = icd9_comorbid_ccs,
                         icd10 = icd10_comorbid_ccs), ...)

#' @describeIn comorbid Compute AHRQ Clinical Classifications Software (CCS) scores
#' @template ccs-single
#' @param lvl If multiple level CCS, then level must be selected as a number
#'   between one and four.
#' @export
icd9_comorbid_ccs <- function(x, ...,
                              single = TRUE,
                              lvl = NULL,
                              map = icd9_map_single_ccs,
                              short_map = TRUE) {
  assert_flag(single)
  assert_int(lvl, lower = 1, upper = 4, null.ok = TRUE, na.ok = FALSE)
  if (!single) {
    if (!is.null(lvl))
      map <- icd9_map_multi_ccs[[paste0("lvl", lvl)]]
    else
      stop("If 'single' is false, then 'lvl' must be supplied as 1, 2, 3 or 4")
  }
  icd9_comorbid(x, map = map, short_map = short_map, ...)
}

#' @rdname comorbid
#' @export
icd10_comorbid_ccs <- function(x, ..., single = TRUE, lvl = NULL) {
  assert_flag(single)
  assert_int(lvl, lower = 1, upper = 2, null.ok = TRUE, na.ok = FALSE)
  m <- icd10_map_ccs$single
  if (!single) {
    if (!is.null(lvl))
      m <- icd10_map_ccs[[paste0("lvl", lvl)]]
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
#'   according to Quan/Deyo ICD categories. Synonymouse with
#'   \code{link{comorbid_quan_deyo}} in this release.
#' @export
comorbid_charlson <- function(...)
  comorbid_quan_deyo(...)

#' @describeIn comorbid Implementation of Clinical Classifcations Software from
#'   AHRQ
#' @export
comorbid_ccs <- function(x, ...)
  switch_ver_cmb(x, list(icd9 = icd9_comorbid_ccs,
                         icd10 = .NotYetImplemented), ...)

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
    if (abbrev_names)
      colnames(x)[cr(x)] <- icd::names_elix_abbrev
    else
      colnames(x)[cr(x)] <- icd::names_elix
  } else {
    if (abbrev_names)
      colnames(x)[cr(x)] <- icd::names_elix_htn_abbrev
    else
      colnames(x)[cr(x)] <- icd::names_elix_htn
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
    if (abbrev_names)
      colnames(cbd)[cr(cbd)] <- icd::names_quan_elix_abbrev
    else
      colnames(cbd)[cr(cbd)] <- icd::names_quan_elix
  } else {
    if (abbrev_names)
      colnames(cbd)[cr(cbd)] <- icd::names_quan_elix_htn_abbrev
    else
      colnames(cbd)[cr(cbd)] <- icd::names_quan_elix_htn
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
  if (abbrev_names)
    colnames(cbd)[cr(cbd)] <- icd::names_charlson_abbrev
  else
    colnames(cbd)[cr(cbd)] <- icd::names_charlson

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
    if (abbrev_names)
      colnames(cbd)[cr(cbd)] <- icd::names_ahrq_abbrev
    else
      colnames(cbd)[cr(cbd)] <- icd::names_ahrq
  } else {
    if (abbrev_names)
      colnames(cbd)[cr(cbd)] <- icd::names_ahrq_htn_abbrev
    else
      colnames(cbd)[cr(cbd)] <- icd::names_ahrq_htn
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
