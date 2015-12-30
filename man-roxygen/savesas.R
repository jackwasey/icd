#' @param sas_path single character string containing path or URL for some SAS
#'   code. The source SAS code is stored in data-raw or at the location
#'   specified in URL. This function is internal, since it is used to generate
#'   data which ends up in the distributed package. However, the package user
#'   can verify that the code creates the distributed R data. Also, changes to
#'   the original SAS code can be used to regenerate the R data by the user,
#'   without waiting for a package release.
#' @param sasPath Deprecated. Use the updated function name and \code{sas_path}
