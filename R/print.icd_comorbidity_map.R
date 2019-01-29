#' Print a comorbidity map
#'
#' The default is to summarize by printing the first seven comorbidities, and
#' the first seven codes for each. To print the whole thing, just convert it to
#' a list.
#' @param x a list optionally with class \code{comorbidity_map}
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
#' @keywords internal
#' @export
print.comorbidity_map <- function(x, ..., n_comorbidities = 7, n_codes = 7) {
  stopifnot(is.list(x))
  assert_int(n_comorbidities)
  assert_int(n_codes)
  get_n_or_len <- function(x, n) {
    x[1:ifelse(length(x) < n, length(x), n)]
  }
  message("Showing first ",
          n_comorbidities, "comorbidities, and first",
          n_codes, "of each.")
  print(get_n_or_len(lapply(x, get_n_or_len, n_codes), n_comorbidities), ...)
  if (length(x) > n_comorbidities)
    writeLines("...")
}
