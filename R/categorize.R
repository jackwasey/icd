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
#' This is the function which optimizes the input data for the categorization,
#' and forms the core of the package, along with the C++ matrix code.
#' @param x Data frame containing a column for an 'id' and a column for a code,
#'   e.g., an ICD-10 code.
#' @template mapping
#' @template visit_name
#' @param code_name String with name of column containing the codes.
#' @template return_df
#' @param return_binary Logical value, if \code{TRUE}, the output will be in 0s
#'   and 1s instead of TRUE and FALSE.
#' @param restore_visit_order Logical value, if \code{TRUE}, the default, the
#'   order of the visit IDs will match the order of visit IDs first encountered
#'   in the input data. This takes a third of the time in calculations on data
#'   with tens of millions of rows, so, if the visit IDs will be discarded when
#'   summarizing data, this can be set to \code{FALSE} for a big speed-up.
#' @param comorbid_fun function i.e. the function symbol (not character string)
#'   to be called to do the comorbidity calculation
#' @param factor_fun function symbol to call to generate factors. Default is a
#'   very simple \code{Rcpp} implementation \code{factor_nosort_rcpp}.
#' @keywords internal
#' @examples
#' u <- uranium_pathology
#' m <- icd10_map_ahrq
#' u$icd10 <- decimal_to_short(u$icd10)
#' j <- icd:::categorize(u, m, visit_name = "case", code_name = "icd10")
#' @export
categorize <- function(x,
                       map,
                       visit_name,
                       code_name,
                       return_df = FALSE,
                       return_binary = FALSE,
                       restore_visit_order = TRUE,
                       comorbid_fun = comorbidMatMul,
                       factor_fun = factor_nosort_rcpp,
                       ...) {
  assert_data_frame(x, min.cols = 2, col.names = "unique")
  class(x) <- "data.frame"
  assert_list(map, any.missing = FALSE, min.len = 1, names = "unique")
  assert(check_string(visit_name), check_null(visit_name))
  assert(check_string(code_name), check_null(code_name))
  stopifnot(visit_name %in% names(x))
  stopifnot(code_name %in% names(x))
  map <- lapply(map, as_char_no_warn)
  # need to convert to string and group these anyway, and much easier and
  # pretty quick to do it here than in C++
  visit_was_factor <- is.factor(x[[visit_name]])
  if (visit_was_factor)
    iv_levels <- levels(x[[visit_name]])
  if (nrow(x) == 0) {
    empty_mat_out <- matrix(nrow = 0,
                            ncol = length(map),
                            dimnames = list(character(0), names(map)))
    if (!return_df) return(empty_mat_out)
    if (visit_was_factor)
      row_names <- factor_nosort(character(0), levels = iv_levels)
    else
      row_names <- character(0)
    df_empty_out <- cbind(row_names, as.data.frame(empty_mat_out), stringsAsFactors = visit_was_factor)
    names(df_empty_out)[1] <- visit_name
    rownames(df_empty_out) <- NULL
    return(df_empty_out)
  }
  uniq_visits <- unique(x[[visit_name]]) # factor or vector
  if (!is.character(x[[visit_name]])) # might be quick to keep as factor
    x[[visit_name]] <- as_char_no_warn(x[[visit_name]])
  # start with a factor for the icd codes in x, recode (and drop superfluous)
  # icd codes in the mapping, then do very fast match on integer without need
  # for N, V or E distinction. Char to factor conversion in R is very fast.
  # Implicit unique in next step, which could be costly
  relevant_codes <- if (is.factor(x[[code_name]]))
    intersect(unlist(map, use.names = FALSE), levels(x[[code_name]]))
  else
    intersect(unlist(map, use.names = FALSE), x[[code_name]])
  split_factor <- factor_split_na(x[[code_name]],
                                  levels = relevant_codes,
                                  factor_fun = factor_fun)
  x_ <- x[split_factor$inc_mask, visit_name, drop = FALSE]
  x_[[code_name]] <- split_factor$factor
  # visit either has: no comorbid codes, mixed comorbid and not codes, or all
  # comorbid codes. If no comorbid codes, we're fine as the visit row will be
  # picked up as not in the split factor. Also fine for only comorbid. If mixed,
  # we keep the visit in the comorbid calc, so don't need it at the end.
  # visit_not_comorbid must therefore only have visit where the visit had no
  # comorbidities at all.

  # selecting only those comorbidities with at least one NOT comorbid hardly
  # helps, as many codes are not in maps

  #TODO SLOW %fnin% about 25% quicker than base R equivalent (if working!)
  visit_not_comorbid <- unique(x[x[[visit_name]] %nin% x_[[visit_name]], visit_name])
  map <- lapply(map, function(y) {
    f <- factor_fun(y, levels = relevant_codes)
    f[!is.na(f)]
  })
  mat_comorbid <- comorbid_fun(icd9df = x_,
                               icd9Mapping = map,
                               visitId = visit_name,
                               icd9Field = code_name) #nolint
  # replace dropped rows (which therefore have no comorbidities) TODO SLOW even
  # just creating a large number of patients without comorbidities takes a long
  # time, then also a long time to rbind them, then sort them. A lot of time is
  # actually garbage collection.
  mat_not_comorbid <- matrix(data = FALSE,
                             nrow = length(visit_not_comorbid),
                             ncol = ncol(mat_comorbid),
                             dimnames = list(visit_not_comorbid))
  mat <- rbind(mat_comorbid, mat_not_comorbid)
  # now put the visits back in original order (bearing in mind that they may not
  # have started that way) # TODO SLOW
  if (restore_visit_order) {
    mat_new_row_order <- match(rownames(mat), uniq_visits)
    mat <- mat[order(mat_new_row_order),, drop = FALSE] #nolint
  }
  if (return_binary) mat <- logical_to_binary(mat)
  if (!return_df)
    return(mat)
  if (visit_was_factor)
    row_names <- factor_fun(x = rownames(mat), levels = iv_levels)
  else
    row_names <- rownames(mat)
  df_out <- cbind(row_names, as.data.frame(mat),
                  stringsAsFactors = visit_was_factor,
                  row.names = NULL)
  names(df_out)[1] <- visit_name
  rownames(df_out) <- NULL
  df_out
}

comorbid_common <- function(..., icd_name)
  categorize(..., code_name = icd_name)
