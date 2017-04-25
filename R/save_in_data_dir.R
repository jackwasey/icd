#' Save given variable in package data directory
#'
#' File is named varname.RData with an optional suffix before .RData
#'
#' @param var_name character or symbol, e.g. "myvar" or \code{myvar}, either of
#'   which would find \code{myvar} in the parent environment, and save it as
#'   \code{myvar.RData} in \code{package_root/data}.
#' @param suffix character scalar
#' @param data_path path to data directory, default is data in current directory.
#' @param package_dir character containing the directory root of the package
#'   tree in which to save the data. Default is the current working directory.
#' @param envir environment in which to look for the variable to save
#' @return invisibly returns the data
#' @keywords internal
save_in_data_dir <- function(var_name, suffix = "", data_path = "data",
                             package_dir = getwd(), envir = parent.frame()) {
  checkmate::assertString(suffix)
  stopifnot(exists(var_name, envir = envir))
  var_name <- as.character(substitute(var_name))
  checkmate::assertString(var_name)
  stopifnot(exists(var_name, envir = envir))
  save(list = var_name,
       envir = envir,
       file = file.path(package_dir, data_path,
                        strip(paste0(var_name, suffix, ".RData"))),
       compress = "xz")
  message("Now reload package to enable updated/new data: ", var_name)
  invisible(get(var_name, envir = envir))
}
