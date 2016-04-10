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
#' # compare CHF for ICD-10 mappings from Elixhauser and AHRQ
#' icd_diff_comorbid(icd10_map_elix, icd10_map_ahrq, show = FALSE)[["CHF"]]
#' \dontrun{
#' # default is to show the results in a human readable manner:
#' diff_result <- icd_diff_comorbid(icd9_map_elix, icd9_map_ahrq)[["CHF"]]
#' # show differences for
#' # give full report on all comorbidities for these mappings
#' diff_result <- icd_diff_comorbid(icd9_map_elix, icd9_map_ahrq, show = FALSE)
#'
#' # the following outputs a summary to the console:
#' icd_diff_comorbid(icd9_map_elix, icd9_map_ahrq)
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
