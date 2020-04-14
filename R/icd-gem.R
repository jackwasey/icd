#' Create An ICD-9 / ICD-10 GEM for the Specified Codes
#'
#' Takes a set of ICD-9 or ICD-10 codes as inputs and returns a representation
#' of the General Equivalence Mappings (GEMs) for the target code set.  The
#' input codes may be either diagnosis or procedure codes.  The returned mapping
#' does not take the same structure as the GEMs, but instead takes an equivalent
#' form that is intended to be more amenable for querying.
#'
#' @section Representing the GEMs (uncondensed representation):
#'
#' The form of the mappings returned by \code{icd_gem} is slightly different
#' depending on the value of \code{condense} (note that the form of the mappings
#' is in fact the same whether you are converting from ICD-9 codes to ICD-10
#' codes or in the opposite direction).  We first describe the form of the
#' mappings for when \code{condense} has a value of \code{FALSE}, and then later
#' describe the form of the mappings for when \code{condense} has a value of
#' \code{TRUE} (the latter of which are a simple transformation of former).
#'
#' The mappings returned by \code{icd_gem} when \code{condense} has a value of
#' \code{FALSE} take a form where each row in the data frame corresponds to a
#' single scenario for a given code provided in the input to \code{codes}.
#' Thus, there are at least as many rows as the number of elements in
#' \code{codes} since there is at least one row per element, and there will be
#' more rows than elements if any of the codes in \code{codes} have more than
#' one scenario.  The value of \code{source} and \code{scenario} together form a
#' unique key for a given row.
#'
#' The columns in the returned data frame are named \code{source},
#' \code{scenario}, \code{type}, \code{approx}, and \code{codes}, and when the
#' codes are provided in a decimal form then an additional column
#' \code{converted} is provided which shows the converted short form
#' representation of the codes.  For an example of utilizing this representation
#' of the GEMs see the documentation for the \code{\link{icd_gem_9_to_10}}.
#'
#' \itemize{
#'
#'   \item \code{source}: A vector of the same type as the input to
#'     \code{source} providing of ICD codes corresponding to one of the input
#'     codes.
#'
#'   \item \code{scenario}: A character vector providing the scenario for each
#'     row.  In the case where the input source code either doesn't have a value
#'     in the GEM or doesn't have a representation in the target code set,  then
#'     this is given a value of \code{"0"}.
#'
#'   \item \code{type}: A character vector taking possible values
#'     \code{"simple"}, \code{"combination"}, \code{"no mapping"}, or
#'     \code{"not in GEM"}.  A value of \code{"simple"} means that the
#'     corresponding source code and scenario pair requires only a match from a
#'     target code to a single collection of candidate codes (i.e. it is not a
#'     combination code).  A value of \code{"combination"} means that a match
#'     must be found to each of multiple collections of candidate codes (i.e. it
#'     is a combination code).  A value of \code{"no mapping"} means that the
#'     source code doesn't have a representation in the target code set, and a
#'     value of \code{"not in GEM"} means that source code isn't found in the
#'     corresponding GEM.
#'
#'   \item \code{approx}: A logical vector providing whether the mapping is an
#'     exact or an approximate mapping.  In the case where the input source code
#'     either doesn't have a value in the GEM or doesn't have a representation
#'     in the target code set this is given a value of \code{TRUE}.
#'
#'   \item \code{codes}: A list such that each element is a list with zero or
#'     more character vectors of target codes, and where the elements are taken
#'     to have the following semantics.  For a given row (i.e. a source code and
#'     scenario), a target code in the target code set must be found in each
#'     vector in the corresponding element of \code{codes}.  For example,
#'     suppose that we are converting from ICD-9 codes to ICD-10 codes.  Then
#'     for a given row in the data frame, if the contents of \code{codes} is a
#'     single character vector then an ICD-10 code must be found in that vector
#'     for an event to have been observed corresponding to the source ICD-9 code
#'     for that given scenario (however, since there may be multiple scenarios
#'     it is possible that the event can occur based on codes in a different
#'     row).
#'
#' }
#'
#' @section Representing the GEMs (condensed representation):
#'
#' When \code{condense} has a value of \code{TRUE}, then scenarios corresponding
#' to the same code are combined into the same row.  This has the advantage of
#' providing a dataset where the rows have a one-to-one correspondence with the
#' inputs corresponding to \code{codes}.  In this setting, the return object is
#' a data frame with columns named \code{source} and \code{mapping}.
#'
#' \itemize{
#'
#'   \item \code{source}: A vector of the same type as the input to
#'     \code{source} providing of ICD codes corresponding to one of the input
#'     codes.
#'
#'   \item \code{mapping}: A data frame providing with the number of rows equal
#'     to the number of scenarios for the corresponding source code, and the
#'     columns \code{scenario}, \code{type}, \code{approx}, and \code{codes}.
#'     These columns have the same meaning as the corresponding columns in the
#'     uncondensed representation of the GEMs.
#'
#' }
#'
#' @param codes A character vector or subclass of a character vector providing
#'   codes that we wish to obtain the mappings for.  In the event that you have
#'   procedure codes, it is necessary that \code{codes} is a subclass of either
#'   \code{icd9cm_pc}, or \code{icd10cm_pc}.  If \code{codes} represents
#'   diagnosis codes and does not have a subclass known to \code{icd}, then the
#'   function will try to guess whether they are ICD-9 or ICD-10 diagnosis
#'   codes.
#' @param condense Either a value of \code{TRUE} or \code{FALSE} specifying
#'   whether entries should be condensed into a single row.
#' @return A data frame that takes different forms depending on the value of
#'   \code{condense}.
#' @examples
#' # specify short form ICD-9 codes
#' short_codes <- c("0010", "24951", "36570")
#' short_codes_explicit <- as.icd9cm(as.short_diag(c("0010", "24951", "36570")))
#' decimal_codes_explicit <- as.icd9cm(as.decimal_diag(c("001.0", "249.51", "365.70")))
#'
#' # has to guess the type of codes
#' icd_gem(short_codes)
#'
#' # use short form codes
#' icd_gem(short_codes_explicit)
#'
#' # use decimal form codes
#' icd_gem(decimal_codes_explicit)
#'
#' # return the condensed form version
#' icd_gem(decimal_codes_explicit, TRUE)
#' @seealso \code{\link{icd_gem_9_to_10}}.
#' @export
icd_gem <- function(codes, condense = FALSE) {
  UseMethod("icd_gem")
}


#' @describeIn icd_gem Obtain a representation of the GEM, where the direction
#'   of the mapping is guessed in lieu of knowing the type of the input codes.
#'   **Warning:** that this function is only able to guess ICD-9 or ICD-10
#'   _diagnosis_ codes, and it will never guess procedure codes.  In the event
#'   that you do have procedure codes, then you will have to explicitly mark
#'   your data using one of `icd9cm_pc`, `as.icd9cm_pc`, `icd10cm_pc`, or
#'   `as.icd10cm_pc` prior to calling `icd_gem`.
#' @export
icd_gem.default <- function(codes, condense = FALSE) {
  switch(
    guess_version.character(as_char_no_warn(codes)),
    "icd9" = icd_gem.icd9(codes),
    "icd10" = icd_gem.icd10(codes),
    stop("ICD type not known")
  )
}


#' @describeIn icd_gem Obtain a representation of the ICD-9 to ICD-10 GEM.
#' @export
icd_gem.icd9 <- function(codes, condense = FALSE) {
  icd_gem_impl(codes, icd::icd_gem_9_to_10, condense)
}


#' @describeIn icd_gem Obtain a representation of the ICD-9 PC to ICD-10 PCS
#'   GEM.
#' @export
icd_gem.icd9cm_pc <- function(codes, condense = FALSE) {
  icd_gem_impl(codes, icd::icd_gem_9pc_to_10pc, condense)
}


#' @describeIn icd_gem Obtain a representation of the ICD-10 to ICD-9 GEM.
#' @export
icd_gem.icd10 <- function(codes, condense = FALSE) {
  icd_gem_impl(codes, icd::icd_gem_10_to_9, condense)
}


#' @describeIn icd_gem Obtain a representation of the ICD-10 PCS to ICD-9 PC
#'   GEM.
#' @export
icd_gem.icd10cm_pc <- function(codes, condense = FALSE) {
  icd_gem_impl(codes, icd::icd_gem_10pc_to_9pc, condense)
}


#' The Implementation of the ICD GEM Routines
#'
#' See the documentation for \code{icd_gem} for a description of this function.
#'
#' @inheritParams icd_gem
#' @param icd_gem A data frame providing the appropriate GEM.
#' @return A data frame of the form as described in the documentation for
#'   \code{icd_gem}.
#' @noRd
icd_gem_impl <- function(codes, icd_gem, condense = FALSE) {

  # this can be removed if `[.icd9cm_pc` and `[.icd10cm_pc` methods are
  # implemented
  make_recover_source_type_info <- function(codes) {
    function(out_df) {
      attr(out_df$source, "icd_short_diag") <- attr(codes, "icd_short_diag")
      class(out_df$source) <- class(codes)
      out_df
    }
  }

  # return TRUE if `codes` is in short form, or FALSE otherwise
  check_if_short <- function(codes) {
    is_short <- attr(codes, "icd_short_diag", exact = TRUE)
    if (! check_if_bool(is_short)) {
      is_short <- guess_short(codes)
      if (is.na(is_short)) {
        stop("Unable to guess whether 'codes' is in short form or decimal form.")
      }
    }
    is_short
  }

  # return TRUE if `x` is a length-1 logical vector, or FALSE otherwise
  check_if_bool <- function(x) {
    (is.logical(x) && (length(x) == 1L) && (! is.na(x)))
  }

  # check the basic form of the inputs
  if (! is.character(codes)) {
    stop("'codes' must either be or inherit from a character vector")
  }
  if (! is.data.frame(icd_gem)) {
    stop("'icd_gem' must be a data frame")
  }
  if (! check_if_bool(condense)) {
    stop("'condense' must either be TRUE or FALSE")
  }

  # create a function to recover the source column type info
  recover_source_type_info <- make_recover_source_type_info(codes)

  # create the ICD code mapping based on whether we've been given short form or
  # decimal form ICD codes
  if (check_if_short(codes)) {
    icd_map_df <- icd_map_impl_create_map(codes, icd_gem, "source_short", "codes_short")
  }
  else {
    icd_map_df <- icd_map_impl_create_map(codes, icd_gem, "source_decimal", "codes_decimal")
  }

  # clean up rows for codes that couldn't be found in the GEM, and conditionally
  # combine rows that have the same codes for `source` into a single row
  icd_map_df <- icd_map_impl_modify_unmatched_rows(icd_map_df)

  # sort by scenario within an entry, and conditionally condense the entry into
  # a single row
  icd_map_df <- icd_map_impl_modify_within_entries(icd_map_df, condense)

  # normalize the row names and recover the source type information
  row.names(icd_map_df) <- seq_len(nrow(icd_map_df))
  recover_source_type_info(icd_map_df)
}


#' Join ICD GEM Table With Input Codes
#'
#' Left join the input codes with the GEM (i.e. all codes are retained).
#'
#' @param codes A character vector or subclass of a character vector providing
#'   codes that we wish to obtain the mappings for.
#' @param icd_gem A data frame providing a GEM with at least a column named
#'   \code{source}.
#' @param source_nm A string providing the name of the column providing the
#'   source codes.
#' @param codes_nm A string providing the name of the column providing the
#'   output codes.
#' @return A data frame with the same columns as \code{icd_gem} and rows that
#'   have a value for \code{source} that is in \code{codes}.
#' @noRd
icd_map_impl_create_map <- function(codes, icd_gem, source_nm, codes_nm) {
  source_df <- setNames(
    object = data.frame(codes, stringsAsFactors = FALSE),
    nm     = source_nm
  )
  out <- merge(
    x     = source_df,
    y     = icd_gem,
    all.x = TRUE,
    by    = source_nm,
    sort  = FALSE
  )
  setNames(
    object = out[, c(source_nm, "scenario", "type", "approx", codes_nm)],
    nm     = c("source", "scenario", "type", "approx", "codes")
  )
}


#' Fill In Predetermined Values for Entries That Have Missing or NULL Values
#'
#' Since the GEMs don't have missing or \code{NULL} values we can assume that
#' this is the result of codes provided as input that aren't part of the GEM.
#' Missing or \code{NULL} values are filled in with the same values as entries
#' in the original GEMs have that don't have an applicable mapping.
#'
#' @param icd_map_df A data frame of the form of the internal representation of
#'   the GEM.
#' @return A data frame of the same form as `icd_map_df`, but with any missing
#'   or \code{NULL} values replaced.
#' @noRd
icd_map_impl_modify_unmatched_rows <- function(icd_map_df) {
  not_in_gem_idx <- which(is.na(icd_map_df$type))
  if (length(not_in_gem_idx) == 0L) {
    return(icd_map_df)
  }
  icd_map_df$scenario[not_in_gem_idx] <- "0"
  icd_map_df$type[not_in_gem_idx] <- "not in GEM"
  icd_map_df$approx[not_in_gem_idx] <- TRUE
  icd_map_df$codes[not_in_gem_idx] <- list(list())
  icd_map_df
}


#' Perform Within-Entry Modifications
#'
#' Rows are sorted within entries by scenario, and entries are conditionally
#' condensed into a single row based on the value of \code{condense}.  These
#' within-entry steps are performed together to avoid splitting and combining
#' the data frame multiple times.
#'
#' @param icd_map_df A data frame of the form of the internal representation of
#'   the GEM.
#' @param condense Either a value of \code{TRUE} or \code{FALSE} specifying
#'   whether entries should be condensed into a single row.
#' @return A data frame that takes different forms depending on the value of
#'   \code{condense}.  See the documentation for \code{\link{icd_gem}} for
#'   details.
#' @noRd
icd_map_impl_modify_within_entries <- function(icd_map_df, condense) {

  # sort by scenario within an entry
  sort_entry <- function(df) {
    df[order(as.integer(df$scenario)), ]
  }

  # takes a data frame and combines it into a 1-row data frame with columns
  # `source` and `mapping`
  combine_common_entry <- function(df) {
    source_col_idx <- which(names(df) == "source")
    out <- data.frame(
      source           = df$source[1L],
      stringsAsFactors = FALSE
    )
    out$mapping = list(df[, -source_col_idx])
    row.names(out$mapping[[1L]]) <- seq_len(nrow(out$mapping[[1L]]))
    out
  }

  # construct the within-entry transformation.  We always sort by scenario
  # within an entry, but only conditionally condense the entry into a single row
  if (condense) {
    transformation_fn <- function(df) {
      combine_common_entry(sort_entry(df))
    }
  } else {
    transformation_fn <- sort_entry
  }

  entry_list <- split(icd_map_df, icd_map_df$source)
  entry_list <- lapply(entry_list, transformation_fn)
  do.call(rbind, entry_list)
}
