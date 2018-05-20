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
#' @param comorbid_fun function i.e. the function symbol (not character string)
#'   to be called to do the comorbidity calculation
#' @param factor_fun function symbol to call to generate factors. Default is a
#'   very simple \code{Rcpp} implementation \code{factor_nosort_rcpp}.
#' @keywords internal
#' @examples
#' u <- uranium_pathology
#' u$icd10 <- decimal_to_short(u$icd10)
#' j <- icd:::categorize(u, icd10_map_ahrq, visit_name = "case",
#'                       code_name = "icd10")
#' @export
categorize <- function(x,
                       map,
                       visit_name,
                       code_name,
                       return_df = FALSE,
                       return_binary = FALSE,
                       comorbid_fun = comorbidMatMul,
                       factor_fun = factor_nosort_rcpp,
                       ...) {
  assert_data_frame(x, min.cols = 2, col.names = "unique")
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
  visit_not_comorbid <- x[!split_factor$inc_mask, visit_name]
  #TODO SLOW ?fastmatch ?Rcpp
  visit_not_comorbid <- visit_not_comorbid[visit_not_comorbid %nin% x_[[visit_name]]]
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
  mat_comb <- rbind(mat_comorbid, mat_not_comorbid)
  # now put the visits back in original order (bearing in mind that they may not
  # have started that way) # TODO SLOW
  mat_new_row_order <- match(rownames(mat_comb), uniq_visits)
  mat <- mat_comb[order(mat_new_row_order),, drop = FALSE] #nolint
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

comorbid_common <- categorize

#' (re-)factor and split into matched and unmatched elements
#'
#' Critically, this function does not convert factor to character vector and
#' back to factor in order to modify factor levels, resulting in huge speed
#' improvements for long vectors.
#' @examples
#' icd:::factor_nosort_rcpp(c("1", NA)) # NA becomes a level
#' icd:::factor_nosort_rcpp(c("1", "2"), "1") # NA not a level, just dropped!
#' icd:::factor_nosort_rcpp(c("1", "2"), c("1", NA)) # NA IS a level
#' suppressWarnings(
#'   print(icd:::factor_nosort_rcpp(c("1", "2"), c("1", NA, NA)))
#' ) # Two NA levels
#'
#' x <- c("A", "B", "C", "d", "A", "C")
#' levels <- c("A", "B")
#' stopifnot(
#'   identical(factor_split_na(factor(x), levels),
#'             factor_split_na(x, levels))
#' )
#' y <- c("A", NA, "B", "A", NA)
#' yf <- factor(y)
#' yf_na <- factor(y, levels = c("A", NA, "B"), exclude = NULL)
#' stopifnot(
#'   identical(factor_split_na(y, "A"),
#'             factor_split_na(yf, "A"))
#' )
#' stopifnot(
#'   identical(factor_split_na(y, "A"),
#'             factor_split_na(yf_na, "A"))
#' )
#' @keywords internal manip
factor_split_na <- function(x, levels, factor_fun = factor_nosort_rcpp) {
  if (is.factor(x)) {
    xi <- as.integer(x)
    lx <- levels(x)
    any_na_xlevels <- anyNA(lx)
    no_na_xlevels <- if (any_na_xlevels) lx[!is.na(lx)] else lx
    new_level_idx <- match(no_na_xlevels, levels)
    f <- new_level_idx[xi]
    inc_mask <- !is.na(f)
    f <- f[inc_mask]
    attr(f, "levels") <- levels
    class(f) <- "factor"
  } else {
    f <- factor_fun(x, levels)
    inc_mask <- !is.na(f)
    f <- f[inc_mask]
  }
  list(factor = f, inc_mask = inc_mask)
}
