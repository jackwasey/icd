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

#' Print a comorbidity map
#'
#' The default is to summarize by printing the first seven comorbidities, and
#' the first seven codes for each. To print the whole thing, just convert it to
#' a list.
#' @param x a list optionally with class \code{icd_comorbidity_map}
#' @param n_comorbidities single integer, number of comorbidities to print
#' @param n_codes single integer, number of codes per comorbidity to print
#' @param ... further arguments are passed to \code{print}
#' @examples
#' icd9_map_ahrq
#' \dontrun{
#' print(icd9_map_ahrq)
#' print(icd9_map_ahrq, n_comorbidities = 3, n_codes = 3)
#' print.list(icd9_map_ahrq)
#' print(list(icd9_map_ahrq))
#' }
#' @export
print.icd_comorbidity_map <- function(x, ..., n_comorbidities = 7, n_codes = 7) {
  assert_list(x, min.len = 1, names = "unique", any.missing = FALSE, types = "character")
  assert_int(n_comorbidities)
  assert_int(n_codes)

  get_n_or_len <- function(x, n) {
    x[1:ifelse(length(x) < n, length(x), n)]
  }

  message("Showing first ",
          n_comorbidities, "comorbidities, and first",
          n_codes, "of each.")
  lapply(x, get_n_or_len, n_codes) %>%
    get_n_or_len(n_comorbidities) %>%
    print(...)

  if (length(x) > n_comorbidities)
    writeLines("...")
}
