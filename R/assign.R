#' Assign all the data in the package to the calling environment
#'
#' Used by \pkg{icd} to load all the data into its environment. This should not
#' be needed by users.
#' @examples
#' \dontrun{
#' assign_icd_data()
#' library(icd)
#' }
#' @keywords internal
#' @noRd
assign_icd_data <- function(env = parent.frame()) {
  data_names <- ls_icd_data()
  lapply(
    data_names,
    function(x) {
      assign(x, get(x), envir = env)
    }
  )
}

#' List the data in this package
#' @examples
#' \dontrun{
#' ls_icd_data()
#' }
#' @keywords datasets
ls_icd_data <- function()
  utils::data(package = "icd")$results[, "Item"]
