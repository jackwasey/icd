#' Assign all the data in the package to the calling environment
#'
#' Used by \CRANpkg{icd} to load all the data into its environment. This should
#' not be needed by users.
#' @examples
#' \dontrun{
#' icd:::assign_icd_data()
#' ls()
#' }
#' @keywords internal
#' @noRd
assign_icd_data <- function(env = parent.frame()) {
  data_names <- .ls_icd_data()
  lapply(
    data_names,
    function(x) {
      assign(x, get(x), envir = env)
    }
  )
}
