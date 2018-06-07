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

#' take two ICD-9 codes and expand range to include all child codes
#'
#' this is cumbersome code, covering a whole load of edge cases relating to the
#' fact that ICD-9 codes are \strong{not} in numeric order. An alternative
#' strategy would be to list all the ICD9 codes, then a range would just pick
#' out start and finish positions, and return subset of the list. Not all ICD-9
#' codes are valid, including some parent codes which have valid children.
#' However, I expect at least some of these have been used in some billing
#' databases.
#'
#' As with \code{link{icd9ExpandRangeShort}} great care is taken not to include
#' codes which have children not in the range. E.g. "100.9" to "101.1" would
#' _not_ include code "101".
#'
#' \code{onlyReal} default is \code{TRUE} (a change from previous versions)
#' since this is far more likely to be useful to the end user.
#' @examples
#' "4280" %i9s% "4289"
#' "4280" %i9s% "42821"
#' "42799" %i9sa% "42802" # doesn't include 428 or 4280
#' "427.99" %i9da% "428.02"
#' "V80" %i9s% "V810"
#' @templateVar icd9ShortName start,end
#' @template icd9-short
#' @template onlyReal
#' @template short_code
#' @param ex_ambig_start single logical value, if \code{TRUE} the range returned
#'   will not include codes which are explicitly listed in the range, but would
#'   imply a broader range than specified. E.g. \code{V10 \%i9sa\% V1009} would
#'   by default (\code{FALSE}) include \code{V10} even though \code{V10} itself
#'   is parent to everything up to \code{V11}.
#' @param ex_ambig_end single logical, same as \code{ex_ambig_start} but affects
#'   codes at the end of the range. E.g. 99.99 to 101.01 would by default
#'   exclude 101 and 101.0
#' @family ICD-9 ranges
#' @export
expand_range <- function(start, end, ...) {
  assert_scalar(start) # i'll permit numeric but prefer char
  assert_scalar(end)
  UseMethod("expand_range")
}

#' @describeIn expand_range Expand a range of ICD-9 or ICD-10 codes when the
#'   class is not known
#' @details When the class is not known, it must be guessed from the start and
#'   end codes. If this guessing fails, e.g. start is ICD-9 whereas end is
#'   ICD-10, then an error is thrown. Otherwise, the appropriate S3 method is
#'   called.
#' @export
#' @keywords internal
expand_range.character <- function(start, end, short_code = NULL, defined = TRUE, ...) {

  icd_ver <- guess_pair_version(start = start, end = end, short_code = short_code)
  if (icd_ver == "icd9") {
    if (is.null(short_code))
      short_code <- guess_short(c(start, end))
    expand_range.icd9(start, end, short_code = short_code, defined = defined, ...)
  } else {
    # if not ICD-9, must be ICD-10 (for now)
    if (is.null(short_code))
      short_code <- guess_short(c(start, end))
    expand_range.icd10cm(start, end, short_code = short_code, defined = defined, ...)
  }
}

#' Expand range of ICD-10 codes returning only defined codes in ICD-10-CM
#'
#' This will need generalizing to any list of officially defined codes, e.g. WHO
#' or other. There are so many permutations of alphanumeric characters after the
#' decimal place that it would be easy to generate many millions with simple
#' ranges, the vast majority of which would be undefined.
#' @param start character vector of length one containing an ICD code
#' @param end  character vector of length one containing an ICD code
#' @export
#' @keywords internal
expand_range.icd10cm <- function(start, end, short_code = guess_short(c(start, end)),
                                 defined = TRUE, ...) {
  if (!defined)
    stop("expanding ranges of possible (versus defined) ICD-10-CM codes is not yet implemented.
         It will produce a very large number of codes because of permutations of the many
         alphabetic and numeric possibilities after the decimal place.")

  # check whether valid?
  # minimal check for type:
  stopifnot(grepl("[^.]", c(start, end)))

  # for ranges, we can semi-legitimately strip any ".x" part ( but other X
  # values CAN appear later). Quan uses x in position 4, but I'm not aware of
  # any ICD-10 code that does this.

  # deliberately not case sensitive, as this is a Quan quirk:
  if (substr(start, 4, 100) == "x")
    start <- substr(start, 1, 3)

  if (substr(end, 4, 100) == "x")
    end <- substr(end, 1, 3)

  end_kids <- children_defined.icd10cm(end, short_code = TRUE)
  new_end <- end_kids[length(end_kids)]

  # find the start and end code positions in the master list
  pos <- match(c(start, new_end), icd10cm2016[["code"]])
  if (is.na(pos[1])) stop(sprintf("start code '%s' not found", start))
  if (is.na(pos[2])) stop(sprintf("calculated end code '%s' not found", end))
  stopifnot(pos[2] >= pos[1])

  icd10cm2016[pos[1]:pos[2], "code"]
}

#' Expand major codes to range
#'
#' Expand a pair of major codes into a range of major codes. This was previously
#' exported, but is now going to be available only with \code{:::}, and
#' \code{expand_range} will follow. This was never supposed to be a user
#' facing function.
#' @keywords internal
expand_range_major <- function(start, end) {
  UseMethod("expand_range_major")
}

#' @describeIn expand_range_major Expand range of top-level, 'major' codes
#'   of unknown type
#' @keywords internal
#' @export
expand_range_major.default <- function(start, end) {
  icd_ver <- guess_pair_version(start, end, short_code = TRUE)
  if (icd_ver == "icd9")
    expand_range_major.icd9(start, end)
  else
    expand_range_major.icd10cm(start, end)
}

#' @describeIn expand_range_major Expand range of top-level ICD-10 codes
#' @keywords internal
#' @export
expand_range_major.icd10cm <- function(start, end) {
  # codes may have alphabetic characters in 3rd position, so can't just do
  # numeric. This may make ICD-10-CM different from ICD-10 WHO. It also makes
  # generating the lookup table of ICD-10-CM codes potentially circular, since
  # we expand the start to end range of chapter and sub-chapter definitions.
  se <- as_char_no_warn(c(start, end)) %>%
    trim %>%
    toupper
  unique_mjrs <- icd10cm2016$three_digit %>% unique
  if (!is_major.icd10cm(se[[1]]))
    stop("start: ", start, " is not an ICD-10-CM major (three character) code")
  if (!is_major.icd10cm(se[[2]]))
    stop("end: ", end, " is not an ICD-10-CM major (three character) code")
  if (se[[1]] > se[[2]])
    stop(se[[1]], " is after ", se[[2]])
  pos <- match(se, unique_mjrs)
  if (is.na(pos[[1]]))
    stop(se[[1]], " as start not found")
  if (is.na(pos[[2]]))
    stop(se[[2]], " as end not found")
  unique_mjrs[pos[[1]]:pos[[2]]] %>%
    as_char_no_warn() %>%
    as.icd10cm
}

#' @describeIn expand_range Expand a range of ICD-9 codes
#' @export
expand_range.icd9 <- function(start, end,
                              short_code = guess_short(c(start, end)),
                              defined = TRUE,
                              ex_ambig_start = TRUE,
                              ex_ambig_end = TRUE,
                              ...) {
  if (short_code)
    icd9_expand_range_short(start, end, defined,
                            ex_ambig_start, ex_ambig_end)
  else
    icd9_expand_range_decimal(start, end, defined,
                              ex_ambig_start, ex_ambig_end)
}

#' expand range worker
#'
#' Expands a range of short ICD-9 codes, dropping ambiguous codes in the middle
#' of ranges
#'
#' @section Ambiguous terminal parent codes: At the end of the output, we may
#'   not want any higher-level codes at the end which would have children beyond
#'   the specified range. There could be lots of lower level codes at the end,
#'   so we actually have to search the whole list to be sure. One parent code
#'   could have maximum of 110 child codes, so we just search the last 110
#'   (TODO). This means that even if trying to preserve the ambiguous start,
#'   setting \code{ex_ambig_end} will have to kill it, if it spills over.
#' @section Ambiguous starting parent codes: Excluding ambiguous parent codes
#'   from the start is easier than those near the end of the result. Just remove
#'   those codes at the beginning which have children not in the output let us
#'   take the first 5, to cover cases like 100, 101, 102.1, 102.11, 102.2. There
#'   are only so many ways for parent codes to appear (assuming the input vector
#'   is ordered)
#' @keywords internal
icd9_expand_range_worker <- function(start, end, lookup, defined,
                                     ex_ambig_start, ex_ambig_end) {
  assert_string(start)
  assert_string(end)
  assert_environment(lookup$env)
  assert_character(lookup$vec)
  assert_flag(ex_ambig_start)
  assert_flag(ex_ambig_end)
  start_index <- lookup$env[[start]]
  end_index <- lookup$env[[end]]
  assert_integer(start_index, len = 1L)
  if (is.na(start_index[1L]))
    stop(sprintf("start value '%s' not found in look-up table of ICD-9 codes.", start))
  assert_integer(end_index, len = 1L)
  if (is.na(end_index[1L]))
    stop(sprintf("end value '%s' not found in look-up table of ICD-9 codes.", end))
  if (end_index < start_index)
    stop("end code must be greater than or equal to start code")
  if (start == end)
    return(children.icd9(start, short_code = TRUE, defined = defined))
  # this fills most of the output values, but misses children of a high-level end code
  out_env <- vec_to_env_true(lookup$vec[start_index:end_index])
  # do not want to check a load of leaf nodes for children, since they have none. # TODO: pre-calculate
  leaf_env <- vec_to_env_true(icd9cm_billable[["32"]][["code"]])
  is_parent <- function(x, defined) {
    if (!defined)
      return(nchar(x) < 5L)
    is.null(leaf_env[[x]])
  }
  icd_get_missing_kids <- function(code, defined) {
    s_kids <- children.icd9(code, short_code = TRUE, defined = defined)
    s_kids_in <- vapply(s_kids, function(x) !is.null(out_env[[x]]), logical(1))
    s_kids[!s_kids_in]
  }
  # operate on the environment, works by side effects, not return value
  exclude_ambiguous_parent <- function(x, defined) {
    if (!is_parent(x, defined))
      return()
    kids <- icd_get_missing_kids(x, defined)
    if (length(kids) == 0L)
      return()
    suppressWarnings(rm(list = x, envir = out_env))
  }

  if (ex_ambig_end)
    lapply(ls(out_env), exclude_ambiguous_parent, defined)
  lapply(
    children.icd9(end, short_code = TRUE, defined = defined),
    function(x) out_env[[x]] <- TRUE)
  if (!ex_ambig_end && ex_ambig_start)
    lapply(lookup$vec[start_index:(start_index + 5L)], exclude_ambiguous_parent, defined)
  sort_icd.icd9(ls(out_env), short_code = TRUE)
}

#' @rdname expand_range
#' @details  Expand range of short_code-form ICD-9 codes
#' @keywords internal
icd9_expand_range_short <- function(start, end, defined = TRUE,
                                    ex_ambig_start = TRUE,
                                    ex_ambig_end = TRUE) {
  assert_scalar(start) # i'll permit numeric but prefer char
  assert_scalar(end)
  assert_flag(defined)
  assert_flag(ex_ambig_start)
  assert_flag(ex_ambig_end)

  # potentially do some checks on start and end. Determine whether we are doing
  # N, V or E then lookup start and end indices in sysdata.rda lookup tables

  if (defined) {
    stopifnot(is_defined(start, short_code = TRUE), is_defined(end, short_code = TRUE))
    if (icd9_is_n(start) && icd9_is_n(end))
      res <- icd9_expand_range_worker(start, end, icd9_short_n_defined, defined = TRUE,
                                      ex_ambig_start, ex_ambig_end)
    else if (icd9_is_v(start) && icd9_is_v(end))
      res <- icd9_expand_range_worker(start, end, icd9_short_v_defined, defined = TRUE,
                                      ex_ambig_start, ex_ambig_end)
    else if (icd9_is_e(start) && icd9_is_e(end))
      res <- icd9_expand_range_worker(start, end, icd9_short_e_defined, defined = TRUE,
                                      ex_ambig_start, ex_ambig_end)
    else
      stop("mismatch between numeric, V and E types in start and end")
  } else {
    if (icd9_is_n(start) && icd9_is_n(end))
      res <- icd9_expand_range_worker(start, end, icd9_short_n, defined = FALSE,
                                      ex_ambig_start, ex_ambig_end)
    else if (icd9_is_v(start) && icd9_is_v(end))
      res <- icd9_expand_range_worker(start, end, icd9_short_v, defined = FALSE,
                                      ex_ambig_start, ex_ambig_end)
    else if (icd9_is_e(start) && icd9_is_e(end))
      res <- icd9_expand_range_worker(start, end, icd9_short_e, defined = FALSE,
                                      ex_ambig_start, ex_ambig_end)
    else
      stop("mismatch between numeric, V and E types in start and end")
  }
  icd9(as.short_diag(res))
}


#' @describeIn expand_range_major Expand a range of ICD-9 major codes into
#'   major codes
#' @export
expand_range_major.icd9 <- function(start, end, defined = TRUE) {
  assert_scalar(start) # i'll permit numeric but prefer char
  assert_scalar(end)
  assert_flag(defined)
  c <- icd9_extract_alpha_numeric(start)
  d <- icd9_extract_alpha_numeric(end)
  # cannot range between numeric, V and E codes, so ensure same type.
  stopifnot(toupper(c[1]) == toupper(d[1]))
  fmt <- if (icd9_is_v(start)) "%02d" else "%03d"
  majors <- icd9(paste(c[, 1], sprintf(fmt = fmt, c[, 2]:d[, 2]), sep = ""))

  if (defined)
    get_defined.icd9(majors, short_code = TRUE, billable = FALSE)
  else
    majors
}

#' @rdname expand_range
#' @keywords internal
icd9_expand_range_decimal <- function(start, end, defined = TRUE,
                                      ex_ambig_start = TRUE,
                                      ex_ambig_end = TRUE) {
  as.decimal_diag(
    icd9(
      short_to_decimal.icd9(
        icd9_expand_range_short(
          decimal_to_short.icd9(start), decimal_to_short.icd9(end),
          defined = defined,
          ex_ambig_start = ex_ambig_start,
          ex_ambig_end = ex_ambig_end
        )
      )
    )
  )
}

#' @rdname expand_range
#' @export
"%i9da%" <- function(start, end) {
  expand_range.icd9(start, end, short_code = FALSE, defined = FALSE)
}

#' @rdname expand_range
#' @export
"%i9sa%" <- function(start, end) {
  expand_range.icd9(start, end, short_code = TRUE, defined = FALSE)
}

#' @rdname expand_range
#' @export
"%i9d%" <- function(start, end) {
  expand_range.icd9(start, end, short_code = FALSE, defined = TRUE)
}

#' @rdname expand_range
#' @export
"%i9mj%" <- function(start, end) {
  expand_range_major.icd9(start, end, defined = TRUE)
}

#' @rdname expand_range
#' @export
"%i9s%" <- function(start, end) {
  expand_range.icd9(start, end, short_code = TRUE, defined = TRUE)
}

#' expand decimal part of ICD-9 code to cover all possible sub-codes
#'
#' Accepts a single number or character input starting point for
#'   generation of all possible decimal parts of ICD9 code. e.g. giving an empty
#'   input will fill out 111 combinations, e..g .1 .11 .12 .... .2 ....
#' @template mnr
#' @param isE single logical, which if TRUE, treats the minor as part of an E
#'   code (which is one character), as opposed to a V or numeric-only code,
#'   which is two character. Default is \code{FALSE}.
#' @examples
#' \dontrun{
#'   # return all possible decimal parts of ICD9 codes (111 in total)
#'   length(icd:::expand_minor(as.icd9(""), is_e = FALSE))
#'   icd:::expand_minor(as.icd9("1")) # "1"  "10" "11" "12" "13" "14" "15" "16" "17" "18" "19"
#' }
#' @return NA for invalid minor, otherwise a vector of all possible (perhaps
#'   non-existent) sub-divisions.
#' @family ICD-9 ranges
#' @keywords internal manip
expand_minor <- function(mnr, ...) {
  UseMethod("expand_minor")
}

#' @describeIn expand_minor expand minor parts of ICD-9 codes
#' @export
expand_minor.icd9 <- function(mnr, is_e = FALSE) {
  icd9_expand_minor_wrap(mnr, isE = is_e)
}

#' @describeIn expand_minor expand minor parts of ICD-10 codes. Not implemented,
#'   no obvious need, and ICD-10 children can be very complex.
#' @export
expand_minor.icd10 <- function(x) {
  .NotYetImplemented() # nocov
}
