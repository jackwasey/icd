# Copyright (C) 2014 - 2018  Jack O. Wasey
#
# This file is part of icd.
#
# icd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd. If not, see <http:#www.gnu.org/licenses/>.

#nocov start

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
  checkmate::assertString(suffix)
  if (!is.character(var_name))
    var_name <- as.character(substitute(var_name))
  stopifnot(exists(var_name, envir = envir))
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

#nocov end
