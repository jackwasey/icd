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

#' Categorize codes according to a mapping
#'
#' This is the function which prepares the input data for the categorization,
#' and forms the core of the package, along with the C++ matrix code. This is
#' pure data manipulation and generalizable beyond medical data.
#' @param x Data frame containing a column for an 'id' and a column for a code,
#'   e.g., an ICD-10 code.
#' @template mapping
#' @param id_name The name of the `data.frame` field which is the unique
#'   identifier.
#' @param code_name String with name of column containing the codes.
#' @template return_df
#' @param return_binary Logical value, if \code{TRUE}, the output will be in 0s
#'   and 1s instead of TRUE and FALSE.
#' @param restore_id_order Logical value, if \code{TRUE}, the default, the order
#'   of the visit IDs will match the order of visit IDs first encountered in the
#'   input data. This takes a third of the time in calculations on data with
#'   tens of millions of rows, so, if the visit IDs will be discarded when
#'   summarizing data, this can be set to \code{FALSE} for a big speed-up.
#' @param unique_ids Single logical value, if \code{TRUE} then the visit IDs in
#'   column given by \code{id_name} are assumed to be unique. Otherwise, the
#'   default action is to ensure they are unique.
#' @param preserve_id_type Single logical value, if \code{TRUE}, the visit ID
#'   column will be converted back to its original type. The default of
#'   \code{FALSE} means only \code{factors} and \code{character} types are
#'   restored in the returned data frame. For matrices, the row names are
#'   necessarily stored as character vectors.
#' @param comorbid_fun function i.e. the function symbol (not character string)
#'   to be called to do the comorbidity calculation
#' @examples
#' \dontrun{
#' u <- uranium_pathology
#' m <- icd10_map_ahrq
#' u$icd10 <- decimal_to_short(u$icd10)
#' j <- categorize_simple(u, m, id_name = "case", code_name = "icd10")
#' }
#' @md
#' @export
categorize_simple <- function(x, map, id_name, code_name,
                              return_df = FALSE, return_binary = FALSE,
                              restore_id_order = TRUE,
                              unique_ids = FALSE,
                              preserve_id_type = FALSE,
                              comorbid_fun = comorbidMatMulSimple) {
  assert_data_frame(x, min.cols = 2, col.names = "unique")
  class(x) <- "data.frame"
  assert_list(map, min.len = 1, names = "unique")
  assert(check_string(id_name), check_null(id_name))
  assert(check_string(code_name), check_null(code_name))
  stopifnot(id_name %in% names(x))
  stopifnot(code_name %in% names(x))
  stopifnot(is.factor(x[[code_name]]) || is.character(x[[code_name]]))
  map <- lapply(map, as_char_no_warn) # TODO: no longer needed with Simple?
  id_was_factor <- is.factor(x[[id_name]])
  visit_class <- class(x[[id_name]])
  if (id_was_factor) iv_levels <- levels(x[[id_name]])
  if (nrow(x) == 0) {
    empty_mat_out <- matrix(nrow = 0,
                            ncol = length(map),
                            dimnames = list(character(0), names(map)))
    if (!return_df) return(empty_mat_out)
    if (id_was_factor)
      row_names <- factor_nosort(character(0), levels = iv_levels)
    else if (preserve_id_type)
      row_names <- switch(visit_class,
                          "integer" = integer(0),
                          "numeric" = numeric(0),
                          "character" = character(0))
    else
      row_names <- character(0)
    df_empty_out <- cbind(row_names, as.data.frame(empty_mat_out),
                          stringsAsFactors = id_was_factor)
    names(df_empty_out)[1] <- id_name
    rownames(df_empty_out) <- NULL
    return(df_empty_out)
  }
  mat <- comorbid_fun(data = x,
                      map = map,
                      id_name = id_name,
                      code_name = code_name) #nolint
  # TODO: move the following into C++ where we have already hashed the vectors
  if (restore_id_order) {
    uniq_visits <- if (unique_ids)
      x[[id_name]]
    else
      unique(x[[id_name]]) # order if factor vs character?
    mat_new_row_order <- match(rownames(mat), as_char_no_warn(uniq_visits))
    mat <- mat[order(mat_new_row_order),, drop = FALSE] #nolint
  }
  if (return_binary) mat <- logical_to_binary(mat)
  if (!return_df) return(mat)
  # TODO: next step better left to the pure C++ functions?
  if (id_was_factor)
    row_names <- factor_nosort_rcpp(x = rownames(mat), levels = iv_levels)
  else if (preserve_id_type) {
    row_names <- switch(visit_class,
                        "integer" = as.integer(rownames(mat)),
                        "numeric" = as.numeric(rownames(mat)),
                        "character" = as.character(rownames(mat)))
    # TODO: don't convert uniq_visits to char, then convert back!
  } else
    row_names <- rownames(mat)
  df_out <- cbind(row_names, as.data.frame(mat),
                  stringsAsFactors = id_was_factor,
                  row.names = NULL)
  names(df_out)[1] <- id_name
  rownames(df_out) <- NULL
  df_out
}

comorbid_common <- function(..., visit_name, icd_name)
  categorize_simple(..., id_name = visit_name, code_name = icd_name)
