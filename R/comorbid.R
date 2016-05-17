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
#' For ICD-10 codes, this method, it relies on exact matching, but not every of
#' billions of possible ICD-10/ICD-10-CM codes are included in the mappings, so
#' it will likely give incomplete results, without searching for parents of the
#' input codes until a match is found in the map. TODO: this is incomplete! For
#' ICD-10, there are two look-up methods. The classic look-up, as used for ICD-9
#' codes, assumes any possible code is available to match in the comorbidity
#' map. However, for ICD-10-CM, there are too many possible codes, specifying
#' subsequent encounters, laterality, etc., etc., so this is too bulky. However
#' for some mappings, there are exact definitions, e.g. AHRQ seems to specify
#' everything (for a given ICD-10-CM year)
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
#'   pts <- icd_long_data(visit_name = c("2", "1", "2", "3", "3"),
#'                    icd9 = c("39891", "40110", "09322", "41514", "39891"))
#'   icd_comorbid(pts, icd9_map_ahrq, short_code = TRUE) # visit_name is now sorted
#'   pts <- icd_long_data(
#'              visit_name = c("1", "2", "3", "4", "4"),
#'              icd_name = c("20084", "1742", "30410", "41514", "95893"),
#'              date = as.Date(c("2011-01-01", "2011-01-02", "2011-01-03",
#'                "2011-01-04", "2011-01-04")))
#'   pt_hccs <- icd_comorbid_hcc(pts, date_name = "date")

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

  assert(checkString(visit_name), checkNull(visit_name))
  assert(checkString(icd_name), checkNull(icd_name))
  visit_name <- get_visit_name(x, visit_name)
  icd_name <- get_icd_name(x, icd_name)
  assert_string(visit_name)
  assert(checkFlag(short_code), checkNull(short_code))
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
#' stopifnot(identical(
#' icd:::icd10_comorbid_parent_search_orig(up, icd10_map_ahrq,
#'   visit_name = "case", icd_name = "icd10",
#'   short_code = FALSE, short_map = TRUE, return_df = FALSE),
#' icd:::icd10_comorbid_parent_search_use_cpp(up, icd10_map_ahrq,
#'   visit_name = "case", icd_name = "icd10",
#'   short_code = FALSE, short_map = TRUE, return_df = FALSE)
#' ))
#' \dontrun{
#' library(microbenchmark)
#' library(stringr)
#' microbenchmark(substr("12345", 1, 4), substring("12345", 1, 4),
#'                str_sub("12345", 1, 4), times = 1e5)
#' # substr is fastest by a good margin
#'
#' microbenchmark(
#'   icd10_comorbid_parent_search_str(uranium_pathology, icd10_map_ahrq,
#'     visit_name = "case", icd_name = "icd10",
#'     short_code = FALSE, short_map = TRUE, return_df = FALSE),
#'   icd10_comorbid_parent_search_use_cpp(uranium_pathology, icd10_map_ahrq,
#'     visit_name = "case", icd_name = "icd10",
#'     short_code = FALSE, short_map = TRUE, return_df = FALSE),
#'   icd10_comorbid_parent_search_all_at_once(uranium_pathology, icd10_map_ahrq,
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

icd10_comorbid_parent_search_str <- function(
  x,
  map,
  visit_name = NULL,
  icd_name = get_icd_name(x),
  short_code = icd_guess_short(x, icd_name = icd_name),
  short_map = icd_guess_short(map),
  return_df = FALSE, ...) {

  if (!short_code)
    x[[icd_name]] <- icd_decimal_to_short.icd10(x[[icd_name]])

  icd_codes <- x[[icd_name]]

  # for each icd code
  just_cmb <- vapply(icd_codes, FUN.VALUE = logical(length(map)), FUN = function(y) {
    # look it up in each comorbidity, but TODO: once we have a comorbidity for
    # one patient, we don't need to search within it again

    char_count <- nchar(as.character(y)):3
    vapply(names(map), FUN.VALUE = logical(1),
           FUN = function(cmb) {
             # and if not found, slice off last char of test string
             for (n in char_count) {
               if (!is.na(fmatch(substr(y, 1, n), map[[cmb]])))
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

icd10_comorbid_parent_search_all_at_once <- function(x,
                                                     map,
                                                     visit_name = NULL,
                                                     icd_name = get_icd_name(x),
                                                     short_code = icd_guess_short(x, icd_name = icd_name),
                                                     short_map = icd_guess_short(map),
                                                     return_df = FALSE, ...) {

  if (!short_code)
    x[[icd_name]] <- icd_decimal_to_short.icd10(x[[icd_name]])

  icd_codes <- x[[icd_name]]

  # for each icd code
  just_cmb <- vapply(icd_codes, FUN.VALUE = logical(length(map)), FUN = function(y) {
    # look it up in each comorbidity, but TODO: once we have a comorbidity for
    # one patient, we don't need to search within it again


    char_count <- nchar(as.character(y)):3
    vapply(names(map), FUN.VALUE = logical(1),
           FUN = function(cmb) {
             # and if not found, slice off last char of test string
             perms_to_match <- vapply(char_count:3, substr, x = y, start = 1, FUN.VALUE = character(1))
             any(fmatch(perms_to_match, map[[cmb]], nomatch = 0L) != 0)
           })
  })

  res <- aggregate(x = t(just_cmb), by = x[visit_name], FUN = any)
  if (return_df)
    return(res)

  out <- as.matrix(res[-1])
  rownames(out) <- res[[1]]
  out
}

icd10_comorbid_parent_search_no_loop <- function(x,
                                                 map,
                                                 visit_name = NULL,
                                                 icd_name = get_icd_name(x),
                                                 short_code = icd_guess_short(x, icd_name = icd_name),
                                                 short_map = icd_guess_short(map),
                                                 return_df = FALSE, ...) {

  if (!short_code)
    x[[icd_name]] <- icd_decimal_to_short.icd10(x[[icd_name]])

  icd_codes <- x[[icd_name]]

  # TODO: unclear whether the hash map is retained by fastmatch: "The first
  # match against a table results in a hash table to be computed from the table.
  # This table is then attached as the '.match.hash' attribute of the table so
  # that it can be re-used on subsequent calls to fmatch with the same table."
  # May need to pre-compute this when building package. May still be faster even
  # if calculating hash map each time.

  # for each icd code
  just_cmb <- vapply(icd_codes, FUN.VALUE = logical(length(map)), FUN = function(y) {
    # look it up in each comorbidity, but TODO: once we have a comorbidity for
    # one patient, we don't need to search within it again

    vapply(names(map), FUN.VALUE = logical(1),
           FUN = function(cmb) {
             # instead of loop, just declare the substring length
             if (!is.na(fmatch(substr(y, 1, 10), map[[cmb]]))) return(TRUE)
             if (!is.na(fmatch(substr(y, 1, 9), map[[cmb]]))) return(TRUE)
             if (!is.na(fmatch(substr(y, 1, 8), map[[cmb]]))) return(TRUE)
             if (!is.na(fmatch(substr(y, 1, 7), map[[cmb]]))) return(TRUE)
             if (!is.na(fmatch(substr(y, 1, 6), map[[cmb]]))) return(TRUE)
             if (!is.na(fmatch(substr(y, 1, 5), map[[cmb]]))) return(TRUE)
             if (!is.na(fmatch(substr(y, 1, 4), map[[cmb]]))) return(TRUE)
             if (!is.na(fmatch(substr(y, 1, 3), map[[cmb]]))) return(TRUE)
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

icd10_comorbid_parent_search_orig <- function(x,
                                              map,
                                              visit_name = NULL,
                                              icd_name = get_icd_name(x),
                                              short_code = icd_guess_short(x, icd_name = icd_name),
                                              short_map = icd_guess_short(map),
                                              return_df = FALSE, ...) {

  if (!short_code)
    x[[icd_name]] <- icd_decimal_to_short.icd10(x[[icd_name]])

  icd_codes <- x[[icd_name]]

  # for each icd code
  just_cmb <- vapply(icd_codes, FUN.VALUE = logical(length(map)), FUN = function(y) {
    # look it up in each comorbidity, but TODO: once we have a comorbidity for
    # one patient, we don't need to search within it again


    char_count <- nchar(as.character(y)):3
    vapply(names(map), FUN.VALUE = logical(1),
           FUN = function(cmb) {
             # and if not found, slice off last char of test string
             for (n in char_count) {
               if (!is.na(fmatch(str_sub(y, 1, n), map[[cmb]])))
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
icd10_comorbid_no_parent_search <- function(x,
                                            map,
                                            visit_name = NULL,
                                            icd_name = get_icd_name(x),
                                            short_code = icd_guess_short(x, icd_name = icd_name),
                                            short_map = icd_guess_short(map[[1]]),
                                            return_df = FALSE, ...) {

  # confirm class is ICD-9 so we dispatch correctly. The class may not be set if
  # the S3 method was called directly.
  if (!is.icd10(x[[icd_name]])) x[[icd_name]] <- icd10(x[[icd_name]]) # no as.icd10
  icd_comorbid_common(x, map, visit_name, icd_name,
                      short_code, short_map, return_df, ...)

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
  assert(checkString(visit_name), checkNull(visit_name))
  assert(checkString(icd_name), checkNull(icd_name))
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

#' @rdname icd_comorbid
#' @details The common comorbidity calculation code does not depend on ICD type.
#'   There is some type conversion so the map and input codes are all in 'short'
#'   format, fast factor generation, then fast comorbidity assignment.
#' @template abbrev_names
#' @template hierarchy
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
  assert(checkString(visit_name), checkNull(visit_name))
  assert(checkString(icd_name), checkNull(icd_name))
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
  mat <- icd9ComorbidShortCpp(icd9df = x, icd9Mapping = map, visitId = visit_name, icd9Field = icd_name, threads = threads, chunk_size = chunk_size, omp_chunk_size = omp_chunk_size, aggregate = TRUE) # nolint

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
#' @details \code{data.frame}s of patient data may have columns within them which are of class \code{icd9}, \code{icd10} etc.,
#' but do not themselves have a class: therefore, the S3 mechanism for dispatch is not suitable. I may add a wrapper function which
#' looks inside a \code{data.frame} of comorbidities, and dispatches to the appropriate function, but right now the user must
#' call the \code{icd9_} or \code{icd10_} prefixed function directly.
#' @export
icd9_comorbid_ahrq <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd9_comorbid(x, map = icd::icd9_map_ahrq, short_map = TRUE, ...)
  apply_hier_ahrq(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @rdname icd_comorbid
#' @export
icd10_comorbid_ahrq <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd10_comorbid(x, map = icd::icd10_map_ahrq, short_map = TRUE, ...)
  apply_hier_ahrq(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @rdname icd_comorbid
#' @export
icd9_comorbid_elix <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd9_comorbid(x, map = icd::icd9_map_elix, short_map = TRUE, ...)
  apply_hier_elix(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @rdname icd_comorbid
#' @export
icd10_comorbid_elix <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd10_comorbid(x, map = icd::icd10_map_elix, short_map = TRUE, ...)
  apply_hier_elix(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @rdname icd_comorbid
#' @export
icd9_comorbid_quan_elix <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd9_comorbid(x, map = icd::icd9_map_quan_elix, short_map = TRUE, ...)
  apply_hier_quan_elix(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @rdname icd_comorbid
#' @export
icd10_comorbid_quan_elix <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd10_comorbid(x, map = icd::icd10_map_quan_elix, short_map = TRUE, ...)
  apply_hier_quan_elix(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @rdname icd_comorbid
#' @export
icd9_comorbid_quan_deyo <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd9_comorbid(x, map = icd::icd9_map_quan_deyo, short_map = TRUE, ...)
  apply_hier_quan_deyo(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @rdname icd_comorbid
#' @export
icd10_comorbid_quan_deyo <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd10_comorbid(x, map = icd::icd10_map_quan_deyo, short_map = TRUE, ...)
  apply_hier_quan_deyo(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @rdname icd_comorbid
#' @export
icd10_comorbid_quan_deyo <- function(x, ..., abbrev_names = TRUE, hierarchy = TRUE) {
  cbd <- icd10_comorbid(x, map = icd::icd10_map_quan_deyo, short_map = TRUE, ...)
  apply_hier_quan_deyo(cbd, abbrev_names = abbrev_names, hierarchy = hierarchy)
}

#' @rdname icd_comorbid
#' @export
icd9_comorbid_hcc <- function(x,
                              date_name = "date",
                              visit_name = NULL,
                              icd_name = NULL
                              ) {
  assert_data_frame(x, min.cols = 3, col.names = "unique")
  assert(checkString(visit_name), checkNull(visit_name))
  assert(checkString(icd_name), checkNull(icd_name))
  visit_name <- get_visit_name(x, visit_name)
  icd_name <- get_icd_name(x, icd_name)
  assert_string(date_name)
  assert_string(visit_name)
  assert_string(icd_name)

  icd_map <- icd::icd9_map_cc
  # Add column for year
  x$year <- as.numeric(format(x[[date_name]], "%Y"))

  # merge CCs to patient data based on ICD and year drop ICD info
  x <- merge(x, icd::icd9_map_cc, all.x = TRUE)

  # Drop missing CC and convert to numeric
  # Not all ICDs resolve to a CC by definition
  x <- x[!is.na(x$cc), ]
  x$cc <- as.numeric(x$cc)

  # keep id, date, and cc columns only, reorder
  x <- x[, c(visit_name, date_name, "year", "cc")]

  # Keep only unique records
  # Multiple ICDs for a patient can resolve to same CC
  x <- unique(x)

  # Import hierarchy mappings, and duplicate the ifcc column
  # needed for future matching
  hierarchy <- icd::icd_map_cc_hcc
  hierarchy$cc <- hierarchy$ifcc

  # Merge hierarchy rules with patient data
  x <- merge(x, hierarchy, all.x = TRUE)

  # Create a list of dataframes that contain the CCs that will be zeroed out
  todrop <- list()
  for (i in 1:6) {
    todrop[[i]] <- x[!is.na(x$ifcc), c(3, 4, 5 + i)]
  }

  # Rename all dataframes in list to same column names
  # rbind into a single dataframe
  todrop <- lapply(1:length(todrop), function(x) {
    names(todrop[[x]]) <- c(visit_name, date_name, "cc")
    return(todrop[[x]])
    }
  )
  todrop <- do.call(rbind, todrop)

  # Remove all NAs from CC field
  todrop <- todrop[!is.na(todrop$cc), ]

  # Set flag for all of the CCs to be dropped
  todrop$todrop <- TRUE

  # Merge drop flags with patient data
  x <- merge(x, todrop, all.x = TRUE)

  # Drop flagged patients and keep columns of interest
  x <- x[is.na(x$todrop), ]
  x <- x[, c(visit_name, date_name, "cc")]
  names(x) <- c(visit_name, date_name, "hcc")
  x
}

#' @rdname icd_comorbid
#' @export
icd10_comorbid_hcc <- function(x,
                              date_name = "date",
                              visit_name = NULL,
                              icd_name = NULL) {
  assert_data_frame(x, min.cols = 3, col.names = "unique")
  assert(checkString(visit_name), checkNull(visit_name))
  assert(checkString(icd_name), checkNull(icd_name))
  visit_name <- get_visit_name(x, visit_name)
  icd_name <- get_icd_name(x, icd_name)
  assert_string(date_name)
  assert_string(visit_name)
  assert_string(icd_name)

  # Add column for year
  x$year <- as.numeric(format(x[[date_name]], "%Y"))

  # merge CCs to patient data based on ICD and year drop ICD info
  x <- merge(x, icd::icd10_map_cc, all.x = TRUE)

  # Drop missing CC and convert to numeric
  # Not all ICDs resolve to a CC by definition
  x <- x[!is.na(x$cc), ]
  x$cc <- as.numeric(x$cc)

  # keep id, date, and cc columns only, reorder
  x <- x[, c(visit_name, date_name, "year", "cc")]

  # Keep only unique records
  # Multiple ICDs for a patient can resolve to same CC
  x <- unique(x)

  # Import hierarchy mappings, and duplicate the ifcc column
  # needed for future matching
  hierarchy <- icd::icd_map_cc_hcc
  hierarchy$cc <- icd::icd_map_cc_hcc$ifcc

  # Merge hierarchy rules with patient data
  x <- merge(x, hierarchy, all.x = TRUE)

  # Create a list of dataframes that contain the CCs that will be zeroed out
  todrop <- list()
  for (i in 1:6) {
    todrop[[i]] <- x[!is.na(x$ifcc), c(3, 4, 5 + i)]
  }

  # Rename all dataframes in list to same column names
  # rbind into a single dataframe
  todrop <- lapply(1:length(todrop), function(x) {
    names(todrop[[x]]) <- c(visit_name, date_name, "cc")
    return(todrop[[x]])
    }
  )
  todrop <- do.call(rbind, todrop)

  # Remove all NAs from CC field
  todrop <- todrop[!is.na(todrop$cc), ]

  # Set flag for all of the CCs to be dropped
  todrop$todrop <- TRUE

  # Merge drop flags with patient data
  x <- merge(x, todrop, all.x = TRUE)

  # Drop flagged patients and keep columns of interest
  x <- x[is.na(x$todrop), ]
  x <- x[, c(visit_name, date_name, "cc")]
  names(x) <- c(visit_name, date_name, "hcc")
  x
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

#' @details Applying CMS Hierarchical Condition Categories
#'   \code{icd_comorbid_hcc} functions differently from the rest of the
#'   comorbidity assignment functions. This is because CMS publishes a specific
#'   ICD to Condition Category mapping including all child ICDs. In addition,
#'   while these mappings were the same for 2007-2012, after 2013 there are
#'   annual versions. In addition, there is a many:many linkage between ICD and
#'   Condition Categories (CC). Once CCs are assigned, a series of hierarchy
#'   rules (which can also change annually) are applied to create HCCs.
#' @rdname icd_comorbid
#' @param date column representing,  the date each record took place, as in each
#'   year there is a different ICD9/10 to CC mapping). This is only necessary
#'   for HCC mappings.
#' @export
icd_comorbid_hcc <- function(x, icd_name = get_icd_name(x), ...) {
  ver <- icd_guess_version.data.frame(x, icd_name = icd_name)
  if (ver == "icd9")
    icd9_comorbid_hcc(x, icd_name = icd_name, ...)
  else if (ver == "icd10")
    icd10_comorbid_hcc(x, icd_name = icd_name, ...)
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
    colnames(cbd)[cr(cbd)] <- icd::charlsonComorbidNamesAbbrev
  else
    colnames(cbd)[cr(cbd)] <- icd::charlsonComorbidNames

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
