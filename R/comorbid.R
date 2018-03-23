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
  "icd9_map_single_ccs", "icd9_map_multi_ccs"
))

#' Present-on-admission flags
#'
#' See \link{icd_filter_poa} for more details.
#' @keywords character
#' @examples
#'   icd_poa_choices
#' @export
icd_poa_choices <- c("yes", "no", "notYes", "notNo")

#' @rdname icd_in_reference_code
#' @export
#' @examples
#' #%i9in% assumes both test code(s) and reference set of codes are \emph{short}
#' "1024" %i9in% "102"
#' "1024" %i9in% c("102", "1025")
#' c("102", "1024", "1025") %i9in% "102"
#' c("102", "1024", "1025") %i9in% c("1024", "1025")
#' c("102", "1024", "1025") %i9in% c("102", "1024", "1025")
"%i9in%" <- function(icd9, icd9Reference) {
  icd_in_reference_code(icd = icd9, icd_reference = icd9Reference,
                        short_code = TRUE, short_reference = TRUE)
}

#' Find comorbidities from ICD-9 codes.
#'
#' This is the main function which extracts comorbidities from a set of ICD-9
#' codes. This is when some trivial post-processing of the comorbidity data is
#' done, e.g. renaming to human-friendly field names, and updating fields
#' according to rules. The exact fields from the original mappings can be
#' obtained using \code{hierarchy = FALSE}, but for comorbidity counting,
#' Charlson Score, etc., the rules should be applied.
#'
#' For ICD-10 codes, this method, it relies on exact matching, but not every of
#' billions of possible ICD-10/ICD-10-CM codes are included in the mappings, so
#' it will likely give incomplete results, without searching for parents of the
#' input codes until a match is found in the map.
#' @param map list (or name of a list if character vector of length one is given
#'   as argument) of the comorbidities with each top-level list item containing
#'   a vector of decimal ICD-9 codes. This is in the form of a list, with the
#'   names of the items corresponding to the comorbidities (e.g. 'HTN', or
#'   'diabetes') and the contents of each list item being a character vector of
#'   short-form (no decimal place but ideally zero left-padded) ICD-9 codes. No
#'   default: user should prefer to use the derivative functions, e.g.
#'   \code{icd_comorbid_ahrq}, since these also provide appropriate naming for
#'   the fields, and squashing the hierarchy (see \code{hierarchy} below)
#' @template visit_name
#' @template icd_name
#' @template short_code
#' @param short_map Same as short, but applied to \code{map} instead of the data
#'   frame of ICD codes, \code{x}. All the codes in a mapping should be of the
#'   same type, i.e. short or decimal.
#' @details The order of visits may change depending on the original sequence,
#'   and the underlying algorithm used. Usually this would be the order of the
#'   first occurence of each visit/patient identifier.
#'
#'   The threading of the C++ can be controlled using e.g.
#'   \code{option(icd.threads = 4)}. If it is not set, the number of cores in
#'   the machine is used. 'OpenMP' environment variables also work.
#' @examples
#'   pts <- icd_long_data(visit_name = c("2", "1", "2", "3", "3"),
#'                    icd9 = c("39891", "40110", "09322", "41514", "39891"))
#'   icd_comorbid(pts, icd9_map_ahrq, short_code = TRUE) # visit_name is now sorted
#'   pts <- icd_long_data(
#'              visit_name = c("1", "2", "3", "4", "4"),
#'              icd_name = c("20084", "1742", "30410", "41514", "95893"),
#'              date = as.Date(c("2011-01-01", "2011-01-02", "2011-01-03",
#'                "2011-01-04", "2011-01-04")))
#'   pt_hccs <- icd_comorbid_hcc(pts, date_name = "date")
#'
#'   pts10 <- icd_long_data(
#'     visit_name = c("a", "b", "c", "d", "e"),
#'     icd_name = c("I058", NA, "T82817A", "", "I69369"),
#'     date = as.Date(
#'       c("2011-01-01", "2011-01-02", "2011-01-03", "2011-01-03", "2011-01-03")))
#'
#'   icd10_comorbid(pts10, map = icd10_map_ahrq)
#'
#' @export
icd_comorbid <- function(x, map, ...) {
  ver <- icd_guess_version.character(map[[1]])
  if (ver == "icd9")
    icd9_comorbid(x, map = map, ...)
  else if (ver == "icd10")
    icd10_comorbid(x, map = map, ...)
  else
    stop("could not guess the ICD version")
}

#' @describeIn icd_comorbid ICD-10 comorbidities
#' @export
icd10_comorbid <- function(x,
                           map,
                           visit_name = NULL,
                           icd_name = NULL,
                           short_code = NULL,
                           short_map = icd_guess_short(map),
                           return_df = FALSE, ...) {

  # need a new way of looking up co-morbidity by string matching. This is
  # annoying, but necessary, since there is a very large number of ICD-10-CM (not
  # so much WHO) codes which are too numerous to pre-compute.
  #
  # options are:
  #
  # 1. do recompute, but only after installing package.
  #
  # 2. do string matching looking for target, then successive parents in the
  # comorbidities
  #
  # 2b. use a very fast lookup table for this, don't loop through the
  # comorbidities.

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
    short_code <- icd_guess_short(x[[icd_name]])

  icd10_comorbid_parent_search(x = x, map = map, visit_name = visit_name, icd_name = icd_name,
                               short_code = short_code, short_map = short_map, return_df = return_df, ...)
}


#' find ICD-10 comorbidities by checking parents
#'
#' @examples
#' up <- uranium_pathology[1:50, ]
#' if (exists("icd10_comorbid_parent_search_orig"))
#'   stopifnot(identical(
#'   icd:::icd10_comorbid_parent_search_orig(up, icd10_map_ahrq,
#'     visit_name = "case", icd_name = "icd10",
#'     short_code = FALSE, short_map = TRUE, return_df = FALSE),
#'   icd:::icd10_comorbid_parent_search_use_cpp(up, icd10_map_ahrq,
#'     visit_name = "case", icd_name = "icd10",
#'     short_code = FALSE, short_map = TRUE, return_df = FALSE)
#'   ))
#' \dontrun{
#' library(microbenchmark)
#' microbenchmark(substr("12345", 1, 4), substring("12345", 1, 4),
#'                stringr::str_sub("12345", 1, 4), times = 1e5)
#' # substr is fastest by a good margin
#'
#' microbenchmark(
#'   icd10_comorbid_parent_search_str(uranium_pathology, icd10_map_ahrq,
#'     visit_name = "case", icd_name = "icd10",
#'     short_code = FALSE, short_map = TRUE, return_df = FALSE),
#'   icd10_comorbid_parent_search_use_cpp(uranium_pathology, icd10_map_ahrq,
#'     visit_name = "case", icd_name = "icd10",
#'     short_code = FALSE, short_map = TRUE, return_df = FALSE),
#'   icd10_comorbid_parent_search_all(uranium_pathology, icd10_map_ahrq,
#'     visit_name = "case", icd_name = "icd10",
#'     short_code = FALSE, short_map = TRUE, return_df = FALSE),
#'   icd10_comorbid_parent_search_no_loop(uranium_pathology, icd10_map_ahrq,
#'     visit_name = "case", icd_name = "icd10",
#'     short_code = FALSE, short_map = TRUE, return_df = FALSE),
#'   icd10_comorbid_parent_search_orig(uranium_pathology, icd10_map_ahrq,
#'     visit_name = "case", icd_name = "icd10",
#'     short_code = FALSE, short_map = TRUE, return_df = FALSE),
#'   times = 3)
#' }
#' @keywords internal
icd10_comorbid_parent_search <- function(
  x,
  map,
  visit_name = NULL,
  icd_name = get_icd_name(x),
  short_code = icd_guess_short(x, icd_name = icd_name),
  short_map = icd_guess_short(map),
  return_df = FALSE, ...) {
  # use the CPP version by default
  icd10_comorbid_parent_search_use_cpp(x = x, map = map, visit_name = visit_name,
                                       icd_name = icd_name, short_code = short_code,
                                       short_map = short_map, return_df = return_df, ...)
}

icd10_comorbid_parent_search_use_cpp <- function(x,
                                                 map,
                                                 visit_name = NULL,
                                                 icd_name = get_icd_name(x),
                                                 short_code = icd_guess_short(x, icd_name = icd_name),
                                                 short_map = icd_guess_short(map),
                                                 return_df = FALSE, ...) {

  if (!short_code)
    x[[icd_name]] <- icd_decimal_to_short.icd10(x[[icd_name]])

  intermed <- icd10_comorbid_parent_search_cpp(x = x, map = map, visit_name = visit_name, icd_name = icd_name)

  res <- aggregate(x = intermed, by = x[visit_name], FUN = any)
  if (return_df)
    return(res)

  out <- as.matrix(res[-1])
  rownames(out) <- res[[1]]
  out
}

#' @describeIn icd_comorbid Get comorbidities from \code{data.frame} of ICD-9
#'   codes
#' @export
icd9_comorbid <- function(x,
                          map,
                          visit_name = NULL,
                          icd_name = NULL,
                          short_code = icd_guess_short(x, icd_name = icd_name),
                          short_map = icd_guess_short(map),
                          return_df = FALSE, ...) {
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

  # confirm class is ICD-9 so we dispatch correctly.
  if (!is.icd9(x[[icd_name]]))
    x[[icd_name]] <- icd9(x[[icd_name]])

  icd_comorbid_common(x = x, map = map, visit_name = visit_name,
                      icd_name = icd_name, short_code = short_code,
                      short_map = short_map, return_df = return_df, ...)
}

#' Internal function to calculate co-morbidities.
#'
#' This common comorbidity calculation code does not depend on ICD type. There
#' is some type conversion so the map and input codes are all in 'short' format,
#' fast factor generation, then fast comorbidity assignment. The trick is to
#' reduce the problem by only looking up comorbidities for codes which actually
#' appear in the data set.
#'
#' 1. get list of ICD codes in the patient data, e.g. Vermont data has 1825, but
#' e.g. only 336 which appear in AHRQ map.
#'
#' 2. make the patient data ICD codes a factor (if not already)
#'
#' 3. drop all elements of the comorbidity map which are not in the patient
#' data, about 336 out of 15000 remain (using AHRQ map as example)
#'
#' 4. drop all patient data where the ICD code is not in the comorbidity map
#'
#' 4a. care here because we need to return blank rows where a patient already
#' didn't have any comorbidities (from the map).
#'
#' 4b. return either in same order provided (making sure we use 'aggregate' when
#' calling C++ so there is only one result for each patient when there are
#' out-of-order patients in the data)
#'
#' 5. call the C++ making sure the map and patient icd codes share the same
#' factor levels
#' @inheritParams icd9_comorbid
#' @param comorbid_fun function i.e. the function (not character string) to be
#'   called to do the comorbidity calculation
#' @keywords internal
icd_comorbid_common <- function(x,
                                map,
                                visit_name = NULL,
                                icd_name,
                                short_code,
                                short_map,
                                return_df = FALSE,
                                comorbid_fun = icd9ComorbidShortCpp, ...) {
  assert_data_frame(x, min.cols = 2, col.names = "unique")
  assert_list(map, any.missing = FALSE, min.len = 1, unique = TRUE, names = "unique")
  assert(check_string(visit_name), check_null(visit_name))
  assert(check_string(icd_name), check_null(icd_name))
  visit_name <- get_visit_name(x, visit_name)
  icd_name <- get_icd_name(x, icd_name)
  assert_string(visit_name)
  assert_flag(short_code)
  assert_flag(short_map)

  stopifnot(visit_name %in% names(x))

  if (!short_code)
    x[[icd_name]] <- icd_decimal_to_short(x[[icd_name]])

  map <- lapply(map, as_char_no_warn)

  if (!short_map)
    map <- lapply(map, icd_decimal_to_short)

  # we need to convert to string and group these anyway, and much easier and
  # pretty quick to do it here:
  visit_was_factor <- is.factor(x[[visit_name]])

  if (visit_was_factor)
    iv_levels <- levels(x[[visit_name]]) # maybe superfluous as we rebuild at end?

  if (nrow(x) == 0) {
    empty_mat_out <- matrix(nrow = 0,
                            ncol = length(map),
                            dimnames = list(character(0), names(map)))
    if (return_df) {
      if (visit_was_factor)
        rownm <- factor(character(0), levels = iv_levels)
      else
        rownm <- character(0)
      df_out <- cbind(rownm, as.data.frame(empty_mat_out), stringsAsFactors = visit_was_factor)
      names(df_out)[1] <- visit_name
      rownames(df_out) <- NULL
      return(df_out)
    } else {
      return(empty_mat_out)
    }
  }

  # may be slow for big data. `rle` might be quicker if we know that
  # patient-visit rows are always contiguous.
  uniq_visits <- unique(x[[visit_name]])
  x[[visit_name]] <- as_char_no_warn(x[[visit_name]])

  # start with a factor for the icd codes in x, recode (and drop superfluous)
  # icd codes in the mapping, then do very fast match on integer without need
  # for N, V or E distinction. Char to factor conversion in R is very fast.
  relevant_codes <- intersect(
    unique(x[[icd_name]]),
    unique(unlist(map, use.names = FALSE))
  )

  # Internally, the \code{sort} is slow. 'fastmatch' inside factor creation
  # would speed up the next step but is unstable, and this is not a bottleneck
  # anyway.
  fac <- factor(x[[icd_name]], levels = relevant_codes)
  x[[icd_name]] <- fac
  # get the visits where there is at least one code which is not in comorbidity
  # map. This is the inverse of all the visits with codes with at least one non-NA value.
  vtmp <- aggregate(
    x[[icd_name]],
    by = list(visit_name = x[[visit_name]]),
    simplify = TRUE,
    FUN = function(y) all(is.na(y))
  )
  visit_not_comorbid <- vtmp[vtmp$x, "visit_name"]

  x <- x[!is.na(fac), ]

  # again, R is very fast at creating factors from a known set of levels. Base
  # `factor` doesn't sort the levels if the levels are given
  map <- lapply(map, function(y) {
    f <- factor_nosort(y, levels = relevant_codes)
    f[!is.na(f)]
  })

  # We can now do pure integer matching for icd9 codes. The only string manip
  # becomes (optionally) defactoring the visit_name for the matrix row names.
  mat_comorbid <- comorbid_fun(icd9df = x, icd9Mapping = map, visitId = visit_name,
                               icd9Field = icd_name,
                               threads = getOption("icd.threads", getOmpCores()),
                               chunk_size = getOption("icd.chunk_size", 256L),
                               omp_chunk_size = getOption("icd.omp_chunk_size", 1L),
                               aggregate = TRUE) # nolint

  # replace dropped rows (which therefore have no comorbidities)
  mat_not_comorbid <- matrix(data = FALSE,
                             nrow = length(visit_not_comorbid),
                             ncol = ncol(mat_comorbid),
                             dimnames = list(visit_not_comorbid))
  mat_comb <- rbind(mat_comorbid, mat_not_comorbid)
  # now put the visits back in original order (bearing in mind that they may not
  # have started that way)
  mat_new_row_order <- match(rownames(mat_comb), uniq_visits)
  mat <- mat_comb[mat_new_row_order,, drop = FALSE] #nolint

  if (return_df) {
    if (visit_was_factor)
      rownm <- factor_nosort(x = rownames(mat), levels = iv_levels)
    else
      rownm <- rownames(mat)
    df_out <- cbind(rownm, as.data.frame(mat), stringsAsFactors = visit_was_factor)
    names(df_out)[1] <- visit_name
    rownames(df_out) <- NULL
    return(df_out)
  }
  mat
}

#' @rdname icd_comorbid
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

#' @rdname icd_comorbid
#' @export
icd10_comorbid_ahrq <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd10_comorbid(x, map = icd10_map_ahrq, short_map = TRUE, ...)
  apply_hier_ahrq(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @rdname icd_comorbid
#' @export
icd9_comorbid_elix <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd9_comorbid(x, map = icd9_map_elix, short_map = TRUE, ...)
  apply_hier_elix(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @rdname icd_comorbid
#' @export
icd10_comorbid_elix <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd10_comorbid(x, map = icd10_map_elix, short_map = TRUE, ...)
  apply_hier_elix(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @rdname icd_comorbid
#' @export
icd9_comorbid_quan_elix <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd9_comorbid(x, map = icd9_map_quan_elix, short_map = TRUE, ...)
  apply_hier_quan_elix(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @rdname icd_comorbid
#' @export
icd10_comorbid_quan_elix <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd10_comorbid(x, map = icd10_map_quan_elix, short_map = TRUE, ...)
  apply_hier_quan_elix(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @rdname icd_comorbid
#' @export
icd9_comorbid_quan_deyo <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd9_comorbid(x, map = icd9_map_quan_deyo, short_map = TRUE, ...)
  apply_hier_quan_deyo(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @rdname icd_comorbid
#' @export
icd10_comorbid_quan_deyo <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd10_comorbid(x, map = icd10_map_quan_deyo, short_map = TRUE, ...)
  apply_hier_quan_deyo(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @rdname icd_comorbid
#' @export
icd10_comorbid_quan_deyo <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd10_comorbid(x, map = icd10_map_quan_deyo, short_map = TRUE, ...)
  apply_hier_quan_deyo(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @rdname icd_comorbid
#' @template ccs-single
#' @param lvl If multiple level CCS, then level must be selected as a number
#'   between one and four.
#' @export
icd9_comorbid_ccs <- function(x, ..., single = TRUE, lvl = NULL ) {
  assert_flag(single)
  assert_int(lvl, lower = 1, upper = 4, null.ok = TRUE, na.ok = FALSE)
  m <- icd9_map_single_ccs
  if (!single) {
    if (!is.null(lvl))
      m <- icd9_map_multi_ccs[[paste0("lvl", lvl)]]
    else
      stop("If 'single' is false, then 'lvl' must be supplied as 1, 2, 3 or 4")
  }
  icd9_comorbid(x, map = m, short_map = TRUE, ...)
}

#' @rdname icd_comorbid
#' @export
icd_comorbid_ahrq <- function(x, icd_name = get_icd_name(x), ...) {
  ver <- icd_guess_version.data.frame(x, icd_name = icd_name)
  if (ver == "icd9")
    icd9_comorbid_ahrq(x, icd_name = icd_name, ...)
  else if (ver == "icd10")
    icd10_comorbid_ahrq(x, icd_name = icd_name, ...)
  else
    stop("could not guess the ICD version using icd_name = ", icd_name)
}

#' @rdname icd_comorbid
#' @export
icd_comorbid_elix <- function(x, icd_name = get_icd_name(x), ...) {
  ver <- icd_guess_version.data.frame(x, icd_name = icd_name)
  if (ver == "icd9")
    icd9_comorbid_elix(x, icd_name = icd_name, ...)
  else if (ver == "icd10")
    icd10_comorbid_elix(x, icd_name = icd_name, ...)
  else
    stop("could not guess the ICD version using icd_name = ", icd_name)
}

#' @rdname icd_comorbid
#' @export
icd_comorbid_quan_elix <- function(x, icd_name = get_icd_name(x), ...) {
  ver <- icd_guess_version.data.frame(x, icd_name = icd_name)
  if (ver == "icd9")
    icd9_comorbid_quan_elix(x, icd_name = icd_name, ...)
  else if (ver == "icd10")
    icd10_comorbid_quan_elix(x, icd_name = icd_name, ...)
  else
    stop("could not guess the ICD version using icd_name = ", icd_name)
}

#' @rdname icd_comorbid
#' @export
icd_comorbid_quan_deyo <- function(x, icd_name = get_icd_name(x), ...) {
  ver <- icd_guess_version.data.frame(x, icd_name = icd_name)
  if (ver == "icd9")
    icd9_comorbid_quan_deyo(x, icd_name = icd_name, ...)
  else if (ver == "icd10")
    icd10_comorbid_quan_deyo(x, icd_name = icd_name, ...)
  else
    stop("could not guess the ICD version using icd_name = ", icd_name)
}

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
      colnames(x)[cr(x)] <- icd::icd_names_elix_abbrev
    else
      colnames(x)[cr(x)] <- icd::icd_names_elix
  } else {
    if (abbrev_names)
      colnames(x)[cr(x)] <- icd::icd_names_elix_htn_abbrev
    else
      colnames(x)[cr(x)] <- icd::icd_names_elix_htn
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
    # icd_comorbid function doesn't attempt to aggregate fields it doesn't know
    # about, e.g. POA, or anything else the user provides in the data frame, so
    # these are just dropped, leaving the fields for visit_name and all the
    # comorbidities:

    if (abbrev_names)
      colnames(cbd)[cr(cbd)] <- icd::icd_names_quan_elix_abbrev
    else
      colnames(cbd)[cr(cbd)] <- icd::icd_names_quan_elix
  } else {
    if (abbrev_names)
      colnames(cbd)[cr(cbd)] <- icd::icd_names_quan_elix_htn_abbrev
    else
      colnames(cbd)[cr(cbd)] <- icd::icd_names_quan_elix_htn
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
    colnames(cbd)[cr(cbd)] <- icd::icd_names_charlson_abbrev
  else
    colnames(cbd)[cr(cbd)] <- icd::icd_names_charlson

  cbd
}

#' @rdname apply_hier
#' @keywords internal manip
apply_hier_ahrq <- function(cbd, abbrev_names = TRUE, hierarchy = TRUE) {
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
      colnames(cbd)[cr(cbd)] <- icd::icd_names_ahrq_abbrev
    else
      colnames(cbd)[cr(cbd)] <- icd::icd_names_ahrq
  } else {
    if (abbrev_names)
      colnames(cbd)[cr(cbd)] <- icd::icd_names_ahrq_htn_abbrev
    else
      colnames(cbd)[cr(cbd)] <- icd::icd_names_ahrq_htn
  }
  cbd
}

#' sequence columns of comorbidities
#'
#' Get sequence of column indices of comorbidity data frame
#' @param cbd matrix or data.frame of comorbidities
#' @keywords internal
cr <- function(x)
  seq(from = 1 + is.data.frame(x), to = ncol(x))
