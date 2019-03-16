# nocov start

#' Save given variable in package data directory
#'
#' File is named \code{varname.RData} with an optional suffix before
#' \code{.RData}
#'
#' @param var_name character name of variable or its symbol either of which
#'   would find \code{myvar} in the parent environment, and save it as
#'   \code{myvar.RData} in \code{package_root/data}.
#' @param suffix character scalar
#' @param data_path path to data directory, default is data in current
#'   directory.
#' @param package_dir character containing the directory root of the package
#'   tree in which to save the data. Default is the current working directory.
#' @param envir environment in which to look for the variable to save
#' @return invisibly returns the data
#' @keywords internal
#' @noRd
save_in_data_dir <- function(var_name, suffix = "", data_path = "data",
                             package_dir = getwd(), envir = parent.frame()) {
  stopifnot(is.character(suffix), is.character(data_path))
  if (!is.character(var_name)) {
    var_name <- as.character(substitute(var_name))
  }
  stopifnot(exists(var_name, envir = envir))
  assert_string(var_name)
  stopifnot(exists(var_name, envir = envir))
  save(
    list = var_name,
    envir = envir,
    file = file.path(
      package_dir, data_path,
      strip(paste0(var_name, suffix, ".RData"))
    ),
    compress = "xz"
  )
  message("Now reload package to enable updated/new data: ", var_name)
  invisible(get(var_name, envir = envir))
}

# nocov end
