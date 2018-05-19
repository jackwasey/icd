#' @param return_binary Single logical value, if \code{TRUE}, the returned
#'   \code{matrix} or \code{data.frame} will be composed of \code{1} and
#'   \code{0}, instead of \code{TRUE} and \code{FALSE}, respectively. This
#'   conversion can also be done by the internal functions
#'   \code{icd:::logical_to_binary} and \code{icd:::binary_to_logical}, or using
#'   other tools, e.g. \code{apply(x, 2, as.integer)}
