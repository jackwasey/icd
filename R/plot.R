#' Basic bar plot showing counts of each comorbidity
#' @param x input patient data
#' @param sort Logical, default `TRUE` which sorts the frequencies from high to
#'   low.
#' @param comorbid_fun Character name of function or function itself, default
#'   being `comorbid_ahrq`
#' @param las Integer, default is 2 which rotates the x axis labels
#'   appropriately
#' @param cex Numeric, default is 0.75 which scales the text size for labels
#'   appropriately
#' @param ... Passed to both `comorbid_fun` and `graphics::barplot`
#' @examples
#' \dontrun{
#' plot_comorbid(icd.data::vermont_dx)
#' plot_comorbid(icd.data::uranium_pathology)
#' }
#' @md
#' @export
plot_comorbid <- function(
  x,
  sort = TRUE,
  comorbid_fun = icd::comorbid_ahrq,
  las = 2,
  cex = 0.75,
  ...
) {
  comorbid_fun <- match.fun(comorbid_fun)
  d <- comorbid_fun(x)
  graphics::barplot(
    sort(colSums(d), decreasing = TRUE),
    las = las, cex.names = cex, ...)
}
