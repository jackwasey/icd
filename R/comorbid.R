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

#' Present-on-admission flags
#'
#' See \link{icd_filter_poa} for more details.
#' @keywords character
#' @examples
#'   icd_poa_choices
#' @export
icd_poa_choices <- c("yes", "no", "notYes", "notNo")

#' @rdname icd_poa_choices
#' @details \code{icd9PoaChoices} is deprecated. Use \code{icd_poa_choices}.
#' @export
icd9PoaChoices <- icd_poa_choices

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
#' @param short_map Same as short, but applied to \code{map} instead of
#'   \code{icd_df}. All the codes in a mapping should be of the same type, i.e.
#'   short or decimal.
#' @details There is a change in behavior from previous versions. The visit_name
#'   column is (implicitly) sorted by using std::set container. Previously, the
#'   visit_name output order was whatever R's \code{aggregate} produced.
#'
#'   The threading of the C++ can be controlled using e.g.
#'   \code{option(icd.threads = 4)}. If it is not set, the number of cores in
#'   the machine is used.
#' @examples
#'   # optional but often helpful
#'   library(magrittr, warn.conflicts = FALSE, quietly = TRUE)
#'
#'   pts <- data.frame(visit_name = c("2", "1", "2", "3", "3"),
#'                    icd9 = c("39891", "40110", "09322", "41514", "39891")) %>%
#'                    icd_long_data %>% icd9
#'    icd_comorbid(pts, icd9_map_ahrq, short_code = TRUE) # visit_name is now sorted
#' @export
icd_comorbid <- function(...)
  UseMethod("icd_comorbid")

#' @describeIn icd_comorbid default method for getting comorbidities, guessing
#'   ICD version. ICD version can be specified by setting the class of the input
#'   data directly, or by calling, for example, \code{icd9(your_data_frame)}.
#' @export
icd_comorbid.default <- function(x, ...) {
  # don't know whether ICD-9 or ICD-10 so we'll guess
  icd_version <- icd_guess_version(x)
  class(x) <- append(icd_version, class(x))
  # dispatch again now class is set. This may be a bad idea: user may not want a
  # class, possibly incorrect, set on their data. At least give a message
  icd_comorbid(x, ...)
}


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

#' @describeIn icd_comorbid Get comorbidities from ICD-10 codes. With this
#'   method, it relies on exact matching, but not every of billions of possible
#'   ICD-10/ICD-10-CM codes are included in the mappings, so it will likely give
#'   incomplete results, without searching for parents of the input codes until
#'   a match is found in the map. TODO: this is incomplete!
#' @details For ICD-10, there are two look-up methods. The classic look-up, as
#'   used for ICD-9 codes, assumes any possible code is available to match in
#'   the comorbidity map. However, for ICD-10-CM, there are too many possible
#'   codes, specifying subsequent encounters, laterality, etc., etc., so this is
#'   too bulky. However for some mappings, there are exact definitions, e.g.
#'   AHRQ seems to specify everything (for a given ICD-10-CM year)
#' @export
icd_comorbid.icd10 <- function(x,
                               map,
                               visit_name = NULL,
                               icd_name = NULL,
                               short_code = NULL,
                               short_map = icd_guess_short.list(map),
                               return_df = FALSE, ...) {

  assert_data_frame(x, min.cols = 2, col.names = "unique")
  assert_list(map, any.missing = FALSE, min.len = 1, unique = TRUE, names = "unique")

  assert(checkmate::checkString(visit_name), checkmate::checkNull(visit_name))
  assert(checkmate::checkString(icd_name), checkmate::checkNull(icd_name))
  visit_name <- get_visit_name(x, visit_name)
  icd_name <- get_icd_name(x, icd_name)
  assert_string(visit_name)
  assert(checkmate::checkFlag(short_code), checkmate::checkNull(short_code))
  assert_flag(short_map)

  if (is.null(icd_name))
    icd_name <- get_icd_name(x)

  if (is.null(short_code))
    short_code <- icd_guess_short.icd10(x[[icd_name]])

  icd10_comorbid_parent_search(x, map, visit_name, icd_name, short_code, short_map, return_df, ...)
}


#' find ICD-10 comorbidities checking parent matches
#'
#' @keywords internal
icd10_comorbid_parent_search <- function(
  x,
  map,
  visit_name = NULL,
  icd_name = get_icd_name(x),
  short_code = icd_guess_short.data.frame(x, icd_name = icd_name),
  short_map = icd_guess_short.list(map),
  return_df = FALSE, ...) {

  if (!short_code)
    x[[icd_name]] <- icd_decimal_to_short.icd10(x[[icd_name]])

  icd_codes <- x[[icd_name]]

  # for each icd code
  just_cmb <- vapply(icd_codes, FUN.VALUE = logical(30), FUN = function(y) {
    # look it up in each comorbidity, but TODO: once we have a comorbidity for
    # one patient, we don't need to search within it again
    vapply(names(icd::icd10_map_ahrq), FUN.VALUE = logical(1),
           FUN = function(cmb) {
             # and if not found, slice off last char of test string
             for (n in nchar(y):3) {
               if (!is.na(fastmatch::fmatch(str_sub(y, 1, n), icd::icd10_map_ahrq[[cmb]])))
                 return(TRUE)
             }
             FALSE
           })
  })

  res <- aggregate(x = t(just_cmb), by = x[visit_name], FUN = any)
  if (return_df)
    return(res)

  out <- as.matrix(res[-1])
  rownames(out) <- res[[1]]
  out
}

#' find ICD-10 comorbidities without checking parent matches
#'
#' @keywords internal
icd10_comorbid_no_parent_search <- function(
  x,
  map,
  visit_name = NULL,
  icd_name = get_icd_name(x),
  short_code = icd_guess_short.data.frame(x, icd_name = icd_name),
  short_map = icd_guess_short.icd10(map[[1]]),
  return_df = FALSE, ...) {

  # confirm class is ICD-9 so we dispatch correctly. The class may not be set if
  # the S3 method was called directly.
  if (!is.icd10(x[[icd_name]])) x[[icd_name]] <- icd10(x[[icd_name]])
  icd_comorbid_common(x, map, visit_name, icd_name,
                      short_code, short_map, return_df, ...)

}

#' @describeIn icd_comorbid Get comorbidities from \code{data.frame} of ICD-9
#'   codes
#' @export
icd_comorbid.icd9 <- function(x,
                              map,
                              visit_name = NULL,
                              icd_name = NULL,
                              short_code = icd_guess_short.data.frame(x, icd_name = icd_name),
                              short_map = icd_guess_short.list(map),
                              return_df = FALSE, ...) {
  assert_data_frame(x, min.cols = 2, col.names = "unique")
  assert_list(map, any.missing = FALSE, min.len = 1, unique = TRUE, names = "unique")
  assert(checkmate::checkString(visit_name), checkmate::checkNull(visit_name))
  assert(checkmate::checkString(icd_name), checkmate::checkNull(icd_name))
  visit_name <- get_visit_name(x, visit_name)
  icd_name <- get_icd_name(x, icd_name)
  assert_string(visit_name)
  assert_string(icd_name)
  assert_flag(short_code)
  assert_flag(short_map)
  # confirm class is ICD-9 so we dispatch correctly. The class may not be set if
  # the S3 method was called directly.
  if (!is.icd9(x[[icd_name]])) x[[icd_name]] <- icd9(x[[icd_name]])

  icd_comorbid_common(x, map, visit_name, icd_name,
                      short_code, short_map, return_df, ...)
}

#' @rdname icd_comorbid
#' @details The common comorbidity calculation code does not depend on ICD type.
#'   There is some type conversion so the map and input codes are all in 'short'
#'   format, fast factor generation, then fast comorbidity assignment.
#' @keywords internal
icd_comorbid_common <- function(x,
                                map,
                                visit_name = NULL,
                                icd_name,
                                short_code,
                                short_map,
                                return_df = FALSE, ...) {
  assert_data_frame(x, min.cols = 2, col.names = "unique")
  assert_list(map, any.missing = FALSE, min.len = 1, unique = TRUE, names = "unique")
  assert(checkmate::checkString(visit_name), checkmate::checkNull(visit_name))
  assert(checkmate::checkString(icd_name), checkmate::checkNull(icd_name))
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

  # new stragegy is to start with a factor for the icd codes in x, recode (and
  # drop superfluous) icd codes in the mapping, then do very fast match on
  # integer without need for N, V or E distinction. Char to factor conversion in
  # R is very fast.

  # this is a moderately slow step (if needed to be done). Internally, the
  # \code{sort} is slow. Fast match speeds up the subsequent step.
  if (!is.factor(x[[icd_name]]))
    x[[icd_name]] <- factor_nosort(x[[icd_name]])

  # we need to convert to string and group these anyway, and much easier and
  # pretty quick to do it here:
  visit_was_factor <- is.factor(x[[visit_name]])

  if (visit_was_factor)
    iv_levels <- levels(x[[visit_name]])

  # this may be the slowest step (again, if needed, and many will have character
  # IDs)
  x[[visit_name]] <- as_char_no_warn(x[[visit_name]])

  # again, R is very fast at creating factors from a known set of levels
  icd_levels <- levels(x[[icd_name]])
  map <- lapply(map, function(y) {
    f <- factor_nosort(y, icd_levels)
    f[!is.na(f)]
  })

  # now we also benefit from only have factor levels in the mapping which appear
  # in the diagnoses we are going to be examining. From the C++ perspective, we
  # can now do pure integer matching for icd9 codes. Only string manip becomes
  # (optionally) defactoring the visit_name for the matrix row names.

  threads <- getOption("icd.threads", getOmpCores())
  chunk_size <- getOption("icd.chunk_size", 256L)
  omp_chunk_size <- getOption("icd.omp_chunk_size", 1L)

  mat <- icd9ComorbidShortCpp(x, map, visit_name, icd_name,
                              threads = threads, chunk_size = chunk_size,
                              omp_chunk_size = omp_chunk_size)

  if (return_df) {
    if (visit_was_factor)
      rownm <- factor_nosort(x = rownames(mat), levels = iv_levels)
    else
      rownm <- rownames(mat)
    df_out <- cbind(rownm, as.data.frame(mat), stringsAsFactors = visit_was_factor)
    names(df_out)[1] <- visit_name
    # perhaps leave (duplicated) rownames which came from the matrix:
    rownames(df_out) <- NULL
    return(df_out)
  }
  mat
}

#' @rdname icd_comorbid
#' @export
icd_comorbid_ahrq <- function(...) {
  UseMethod("icd_comorbid_ahrq")
}

#' @rdname icd_comorbid
#' @export
icd_comorbid_quan_elix <- function(...) {
  UseMethod("icd_comorbid_quan_elix")
}

#' @rdname icd_comorbid
#' @export
icd_comorbid_quan_deyo <- function(...) {
  UseMethod("icd_comorbid_quan_deyo")
}

#' @rdname icd_comorbid
#' @export

icd_comorbid_elix <- function(...) {
  UseMethod("icd_comorbid_elix")
}

#' @rdname icd_comorbid
#' @export
#' @keywords internal
#' @method icd_comorbid_ahrq data.frame
icd_comorbid_ahrq.data.frame <- function(x, ..., icd_name = get_icd_name(x)) {
  x <- update_data_frame_class(x, icd_name, must_work = TRUE)
  UseMethod("icd_comorbid_ahrq", x)
}

#' @rdname icd_comorbid
#' @export
#' @keywords internal
#' @method icd_comorbid_elix data.frame
icd_comorbid_elix.data.frame <- function(x, ..., icd_name = get_icd_name(x)) { # nolint # false +ve
  x <- update_data_frame_class(x, icd_name, must_work = TRUE)
  UseMethod("icd_comorbid_elix", x)
}

#' @rdname icd_comorbid
#' @export
#' @keywords internal
#' @method icd_comorbid_quan_elix data.frame
icd_comorbid_quan_elix.data.frame <- function(x, ..., icd_name = get_icd_name(x)) { # nolint # false +ve
  x <- update_data_frame_class(x, icd_name, must_work = TRUE)
  UseMethod("icd_comorbid_quan_elix", x)
}

#' @rdname icd_comorbid
#' @export
#' @keywords internal
#' @method icd_comorbid_quan_deyo data.frame
icd_comorbid_quan_deyo.data.frame <- function(x, ..., icd_name = get_icd_name(x)) {# nolint # false +ve
  x <- update_data_frame_class(x, icd_name, must_work = TRUE)
  UseMethod("icd_comorbid_quan_deyo", x)
}

#' @rdname icd_comorbid
#' @param abbrev_names  single logical value that defaults to \code{TRUE}, in
#'   which case the shorter human-readable names stored in e.g.
#'   \code{ahrqComorbidNamesAbbrev} are applied to the data frame column names.
#' @param hierarchy single logical value that defaults to \code{TRUE}, in
#'   which case the hierarchy defined for the mapping is applied. E.g. in
#'   Elixhauser, you can't have uncomplicated and complicated diabetes both
#'   flagged.
#' @export
icd_comorbid_ahrq.icd9 <- function(..., abbrev_names = TRUE,
                                   hierarchy = TRUE) {
  assert_flag(abbrev_names)
  assert_flag(hierarchy)

  cbd <- icd_comorbid.icd9(..., map = icd::icd9_map_ahrq)
  icd_comorbid_ahrq_worker(cbd, abbrev_names, hierarchy)
}

#' @rdname icd_comorbid
#' @export
icd_comorbid_ahrq.icd10 <- function(..., abbrev_names = TRUE, hierarchy = TRUE) {
  assert_flag(abbrev_names)
  assert_flag(hierarchy)

  cbd <- icd_comorbid.icd10(..., map = icd::icd10_map_ahrq)
  icd_comorbid_ahrq_worker(cbd, abbrev_names, hierarchy)
}

# lots of duplicated code, need to simplify
icd_comorbid_ahrq_worker <- function(cbd, abbrev_names = TRUE, hierarchy = TRUE) {
  assert_flag(abbrev_names)
  assert_flag(hierarchy)
  if (hierarchy) {

    # Use >0 rather than logical - apparently faster, and future proof against
    # change to binary from logical values in the matirx.
    cbd[cbd[, "Mets"] > 0, "Tumor"] <- FALSE
    cbd[cbd[, "DMcx"] > 0, "DM"] <- FALSE
    cbd[, "HTN"] <- (cbd[, "HTN"] + cbd[, "HTNcx"]) > 0

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

#' @rdname icd_comorbid
#' @details For Deyo's Charlson comorbidities, strictly speaking, there is no
#'   dropping of more e.g. uncomplicated \code{DM} if complicated \code{DM}
#'   exists, however, this is probably useful, in general and is essential when
#'   calculating the Charlson score.
#' @export
icd_comorbid_quan_deyo.icd9 <- function(..., abbrev_names = TRUE,
                                        hierarchy = TRUE) {
  assert_flag(abbrev_names)
  assert_flag(hierarchy)
  cbd <- icd_comorbid.icd9(..., map = icd::icd9_map_quan_deyo)
  if (hierarchy) {
    # Use >0 rather than logical - apparently faster, and future proof against
    # change to binary from logical values in the matirx.
    cbd[cbd[, "Mets"] > 0, "Cancer"] <- FALSE
    cbd[cbd[, "DMcx"] > 0, "DM"] <- FALSE
    cbd[cbd[, "LiverSevere"] > 0, "LiverMild"] <- FALSE
  }
  if (abbrev_names)
    colnames(cbd)[cr(cbd)] <- icd::charlsonComorbidNamesAbbrev
  else
    colnames(cbd)[cr(cbd)] <- icd::charlsonComorbidNames

  cbd
}

#' @rdname icd_comorbid
#' @export
icd_comorbid_quan_elix.icd9 <- function(..., abbrev_names = TRUE,
                                        hierarchy = TRUE) {
  assert_flag(abbrev_names)
  assert_flag(hierarchy)
  cbd <- icd_comorbid.icd9(..., map = icd::icd9_map_quan_elix)
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

#' @rdname icd_comorbid
#' @export
icd_comorbid_elix.icd9 <- function(..., abbrev_names = TRUE, hierarchy = TRUE) {
  assert_flag(abbrev_names)
  assert_flag(hierarchy)
  cbd <- icd_comorbid.icd9(..., map = icd::icd9_map_elix)
  if (hierarchy) {
    cbd[cbd[, "Mets"] > 0, "Tumor"] <- FALSE
    cbd[cbd[, "DMcx"] > 0, "DM"] <- FALSE
    cbd[, "HTN"] <- (cbd[, "HTN"] + cbd[, "HTNcx"]) > 0

    # drop HTNcx without converting to vector if matrix only has one row
    cbd <- cbd[, -which(colnames(cbd) == "HTNcx"), drop = FALSE]

    if (abbrev_names)
      colnames(cbd)[cr(cbd)] <- icd::icd_names_elix_abbrev
    else
      colnames(cbd)[cr(cbd)] <- icd::icd_names_elix
  } else {
    if (abbrev_names)
      colnames(cbd)[cr(cbd)] <- icd::icd_names_elix_htn_abbrev
    else
      colnames(cbd)[cr(cbd)] <- icd::icd_names_elix_htn
  }
  cbd
}

#' show the difference between two comorbidity mappings
#'
#' Compares two comorbidity to ICD code mappings. The results are returned
#' invisibly as a list. Only those comorbidities with (case sensitive)
#' overlapping names are compared.
#' @param x list of character vectors
#' @param y list of character vectors
#' @param all_names character vector of the comorbidity names
#' @param x_names character vector of the comorbidity names from \code{x} to
#'   compare
#' @param y_names character vector of the comorbidity names from \code{y} to
#'   compare
#' @param show single logical value. The default is \code{TRUE} which causes a
#'   report to be printed.
#' @param explain single logical value. The default is \code{TRUE} which means
#'   the differing codes are attempted to be reduced to their parent codes, in
#'   order to give a more succinct summary.
#' @examples
#' \dontrun{
#' diff_result <- icd_diff_comorbid(elixComorbid, ahrqComorbid,
#'                                  show = TRUE)[["CHF"]]
#' # show differences for
#' # give full report on all comorbidities for these mappings
#' diff_result <- icd_diff_comorbid(elixComorbid, ahrqComorbid, show = FALSE)
#'
#' # the following outputs a summary to the console:
#' icd_diff_comorbid(elixComorbid, ahrqComorbid)
#' }
#' @return A list, each item of which is another list containing the
#'   intersections and both asymmetric differences.
#' @export
icd_diff_comorbid <- function(x, y, all_names = NULL, x_names = NULL,
                              y_names = NULL, show = TRUE, explain = TRUE) {
  UseMethod("icd_diff_comorbid")
}

#' @describeIn icd_diff_comorbid Show difference between comorbidity maps with
#'   ICD-9 codes
#' @export
icd_diff_comorbid.list <- function(x, y, all_names = NULL, x_names = NULL,
                                   y_names = NULL, show = TRUE, explain = TRUE) {
  assert_list(x, min.len = 1, any.missing = FALSE,
              types = c("character"), names = "unique")
  assert_list(y, min.len = 1, any.missing = FALSE,
              types = c("character"), names = "unique")
  assert_flag(show)
  assert_flag(explain)
  stopifnot(all(x_names %in% names(x)), all(y_names %in% names(y)))

  lapply(x, function(z) stopifnot(is.character(z)))
  lapply(y, function(z) stopifnot(is.character(z)))

  if (!is.null(names) && (!is.null(x_names) | !is.null(y_names)))
    stop("if 'all_names' is specified, 'x_names' and 'y_names' should not be")

  if (!is.null(all_names))
    x_names <- y_names <- all_names

  if (is.null(x_names)) x_names <- names(x)
  if (is.null(y_names)) y_names <- names(y)

  common.names <- intersect(x_names, y_names)

  x.title <- deparse(substitute(x))
  y.title <- deparse(substitute(y))

  out <- list();

  for (n in common.names) {
    both <- intersect(x[[n]], y[[n]])
    only.x <- setdiff(x[[n]], y[[n]])
    only.y <- setdiff(y[[n]], x[[n]])
    out[[n]] <- list(both = both, only.x = only.x, only.y = only.y)
    if (show) {
      cat(sprintf("Comorbidity %s: ", n))
      if (length(both) == 0) {
        cat("no common codes. ")
      }
      if (length(only.x) == 0 && length(only.y) == 0) {
        cat("match.\n")
        next
      }
      if (length(only.x) > 0) {
        cat(sprintf("\n%s has %d codes not in %s. First few are: ",
                    x.title, length(only.x), y.title))
        lapply(icd_explain(only.x, condense = TRUE, brief = TRUE, warn = FALSE)[1:5],
               function(s) if (!is.na(s)) cat(sprintf("'%s' ", s)))
      }
      if (length(only.y) > 0) {
        cat(sprintf("\n%s has %d codes not in %s. First few are: ",
                    y.title, length(only.y), x.title))
        lapply(icd_explain(only.y, condense = TRUE, brief = TRUE, warn = FALSE)[1:5],
               function(s) if (!is.na(s)) cat(sprintf("'%s' ", s)))
      }
      cat("\n")
    }
  }
  if (show) {
    cmb_only_x <- setdiff(x_names, y_names)
    cmb_only_y <- setdiff(y_names, x_names)

    if (length(cmb_only_x) > 0) {
      cat(sprintf("Comorbidities only defined in %s are: ", x.title))
      lapply(cmb_only_x, function(s) cat(sprintf("%s ", s)))
      cat("\n")
    }

    if (length(cmb_only_y) > 0) {
      cat(sprintf("Comorbidities only defined in %s are: ", y.title))
      lapply(cmb_only_y, function(s) cat(sprintf("%s ", s)))
      cat("\n")
    }
  }
  invisible(out)
}

#' sequence columns of comorbidities
#'
#' Get sequence of column indices of comorbidity data frame
#' @param cbd matrix or data.frame of comorbidities
#' @keywords internal
cr <- function(x)
  seq(from = 1 + is.data.frame(x), to = ncol(x))
