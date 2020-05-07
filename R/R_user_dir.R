#' @title Backport of R_user_dir for R < 4.0.0
#'
#' @description
#' See the original description in \code{tools::R_user_dir}.
#'
#' @keywords internal
#' @rawNamespace if (getRversion() < "4.0.0") export(R_user_dir)
#' @examples
#' # get function from namespace instead of possibly getting
#' # implementation shipped with recent R versions:
#' bp_R_user_dir = getFromNamespace("R_user_dir", "backports")
#'
#' bp_R_user_dir("backports")
R_user_dir <- function(package, which = c("data", "config", "cache")) {
  stopifnot(is.character(package), length(package) == 1L)

  which <- match.arg(which)

  home <- normalizePath("~")

  path <-
    switch(which,
      data = {
        if(nzchar(p <- Sys.getenv("R_USER_DATA_DIR")))
          p
        else if(nzchar(p <- Sys.getenv("XDG_DATA_HOME")))
          p
        else if(.Platform$OS.type == "windows")
          file.path(Sys.getenv("APPDATA"), "R", "data")
        else if(Sys.info()["sysname"] == "Darwin")
          file.path(home, "Library", "Application Support",
            "org.R-project.R")
        else
          file.path(home, ".local", "share")
      },
      config = {
        if(nzchar(p <- Sys.getenv("R_USER_CONFIG_DIR")))
          p
        else if(nzchar(p <- Sys.getenv("XDG_CONFIG_HOME")))
          p
        else if(.Platform$OS.type == "windows")
          file.path(Sys.getenv("APPDATA"), "R", "config")
        else if(Sys.info()["sysname"] == "Darwin")
          file.path(home, "Library", "Preferences",
            "org.R-project.R")
        else
          file.path(home, ".config")
      },
      cache = {
        if(nzchar(p <- Sys.getenv("R_USER_CACHE_DIR")))
          p
        else if(nzchar(p <- Sys.getenv("XDG_CACHE_HOME")))
          p
        else if(.Platform$OS.type == "windows")
          file.path(Sys.getenv("LOCALAPPDATA"), "R", "cache")
        else if(Sys.info()["sysname"] == "Darwin")
          file.path(home, "Library", "Caches",
            "org.R-project.R")
        else
          file.path(home, ".cache")
      })

  file.path(path, "R", package)
}
