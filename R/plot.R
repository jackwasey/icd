#' Basic ordered bar plot showing counts of each comorbidity
#' @param x input patient data
#' @param sort Logical, default \code{TRUE} which sorts the frequencies from
#'   high to low.
#' @param comorbid_fun Character name of function or function itself, default
#'   being \code{comorbid_ahrq}
#' @param fix_margin Logical, default \code{TRUE}, which causes a
#'   \code{\link[graphics]{par}} margin to be set so the x axis labels are less
#'   likely to be truncated.
#' @param las Integer, default is 2 which rotates the x axis labels
#'   appropriately
#' @param cex.names Numeric, default is 0.75 which scales the text size for
#'   labels appropriately
#' @param ... Passed to \code{\link[graphics]{barplot}}
#' @examples
#' \dontrun{
#' plot_comorbid(icd.data::vermont_dx)
#' plot_comorbid(icd.data::uranium_pathology)
#' # Or calculate the comorbidities, then plot the results
#' cmb <- comorbid_ahrq(icd.data::vermont_dx)
#' # plot with full, not abbreviated names
#' plot_comorbid_results(cmb, names.arg = names_ahrq)
#' # or return with full names, and plot those:
#' cmb <- comormbid_ahrq(icd.data::vermont_dx, abbrev_names = FALSE) %>%
#'   plot_comorbid_results()
#' }
#' @keywords hplot
#' @export
plot_comorbid <- function(x,
                          comorbid_fun = icd::comorbid_ahrq,
                          ...) {
  comorbid_fun <- match.fun(comorbid_fun)
  plot_comorbid_results(comorbid_fun(x), ...)
}

#' @describeIn plot_comorbid Plot the results of a call to one of the
#'   comorbidity settings.
#' @export
plot_comorbid_results <- function(x,
                                  sort = TRUE,
                                  fix_margin = TRUE,
                                  las = 2,
                                  cex.names = 0.75,
                                  ...) {
  if (fix_margin) {
    op <- graphics::par(mar = c(12, 4, 4, 2) + 0.1)
    on.exit(graphics::par(op), add = TRUE)
  }
  graphics::barplot(
    sort(colSums(x), decreasing = TRUE),
    las = las,
    cex.names = cex.names,
    ...
  )
}
