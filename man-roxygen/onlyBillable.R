#' @param onlyBillable single logical value, if \code{TRUE}, describes the input
#'   data, stating that it only contains billable codes. Usually, the function
#'   will try to guess this, but if you know in advance what they should be, the
#'   functions can optionally warn if this is incorrect, and save some
#'   computation time. The billable codes are derived from the CMS list. The
#'   most recent version is used by default.
