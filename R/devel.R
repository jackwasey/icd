# /usr/bin/env Rscript

# Various functions for development purposes only, not to be included in the R
# package. Source or (temporarily) include in package

load_dev_pkgs <- function() {
  suppressWarnings(suppressPackageStartupMessages({
    library(devtools)
    library(pkgbuild)
    library(usethis)
    library(testthat)
    library(magrittr)
    library(lintr)
  }))
}

toolchain <- function() {
  mf <- Sys.getenv("MAKEFLAGS", unset = NA)
  if (is.na(mf)) {
    message("MAKEFLAGS is unset. Setting...")
    Sys.setenv(MAKEFLAGS = paste0("-j", parallel::detectCores()))
  }
  message("MAKEFLAGS is now set:")
  print(Sys.getenv("MAKEFLAGS", unset = NA))
  makevars_path <- file.path("~", ".R", "Makevars")
  mkln <- Sys.readlink(makevars_path)
  if (!is.na(mkln)) {
    message("~/.R/Makevars is linked to:")
    print(mkln)
  } else if (file.exists(makevars_path)) {
    message("~/.R/Makevars is present")
  }
  rum <- Sys.getenv("R_USER_MAKEVARS", unset = NA)
  if (is.na(rum)) {
    message("R_USER_MAKEVARS is not set")
  } else {
    message("R_USER_MAKEVARS is set to:")
    print(rum)
  }
}

sitrep <- function() {
  if ("package:icd" %in% search()) {
    message("icd loaded and attached")
  } else {
    if (!("icd" %in% rownames(installed.packages()))) {
      warning("icd not installed (but may nevertheless be loaded via devtools)")
    } else {
      message(
        "icd version installed is: ", installed.packages()["icd", "Version"],
        " in library path ", installed.packages()["icd", "LibPath"]
      )
    }
    if ("icd" %in% base::loadedNamespaces()) {
      message("icd loaded but not attached")
    } else {
      message("icd is installed, but not loaded.")
    }
  }
  if (getAnywhere(".show_options")) {
    message("icd options are:")
    icd:::.show_options()
  } else {
    warning("cannot find icd:::.show_options")
  }
  message("R_MAKEVARS_USER='", Sys.getenv("R_MAKEVARS_USER", ""), "'")
  system2("ls", c("-l", "~/.R/Makevars*"))
}

ihd <- function(quiet = TRUE) {
  if (!exists(".m")) .m <- function(...) message(...)
  ih <- Sys.getenv("IH", unset = NA)
  icd_home <- Sys.getenv("ICD_HOME", unset = NA)
  rs <- if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
    rstudioapi::getActiveProject()
  } else {
    NA
  }
  .m("IH from env is ", ih)
  .m("ICD_HOME from env is ", icd_home)
  .m("R working dir is ", getwd())
  .m("package devel dir is ", devtools::wd())
  .m("RStudio active project dir is ", rs)
  .m("PWD is ", Sys.getenv("PWD", "Not set: probably using RStudio."))
  if (!is.na(ih)) {
    return(ih)
  }
  if (!is.na(icd_home)) {
    return(icd_home)
  }
  wd_test <- NA
  try(
    {
      while (is.na(wd_test) || !identical(getwd(), wd_test)) {
        if (is.na(wd_test)) wd_test <- getwd()
        if (file.exists(file.path(wd_test, "DESCRIPTION"))) {
          return(wd_test)
        }
        setwd("..")
      }
    },
    silent = TRUE
  )
  if (dir.exists(rs)) {
    return(rs)
  }
  stop("Unable to establish working dir for icd devel")
}

ih <- function() {
  setwd(ihd(quiet = TRUE))
}

ihf <- function(..., quiet = TRUE) {
  file.path(ihd(), ...)
}


#' Experimental C function (via C) to get a character vector from a factor. Not
#' faster than base R for long string vectors, slower for short ones.
#'
#' In case of \code{factorAsCharNoNA}, do this with minimal safeguards.
#'
#' For short factors, it is slower than base R, but with necessary overhead for
#' \code{::}. \code{.Call} is still slightly slower, though.
#' @examples
#' \dontrun{
#' icd_ns <- loadNamespace("icd")
#' fac <- get("factor_as_char_no_na", envir = icd_ns)
#' f <- uranium_pathology[["icd10"]]
#' g <- as.factor(icd:::generate_random_short_ahrq_icd9(1E8))
#' stopifnot(identical(icd:::factor_as_char(f), as.character(f)))
#' stopifnot(identical(icd:::factor_as_char(g), as.character(g)))
#' if (suppressPackageStartupMessages(requireNamespace("bench", quietly = TRUE))) {
#'   bench::mark(
#'     icd:::factor_as_char(f),
#'     fac(f),
#'     .Call("factorAsCharNoNa", f, PACKAGE = "icd"),
#'     as.character(f)
#'   )
#' }
#' bench::mark(
#'   icd:::factor_as_char(g),
#'   fac(g),
#'   .Call("factorAsCharNoNa", g, PACKAGE = "icd"),
#'   as.character(g)
#' )
#' }
#'
#' @keywords internal
#' @noRd
factor_as_char <- function(x) {
  .Call("factorAsChar", x, PACKAGE = "icd")
}

#' @describeIn factor_as_char Factor as character vector, assuming no NA values
#' @keywords internal
#' @noRd
factor_as_char_no_na <- function(x) {
  .Call("factorAsCharNoNa", x, PACKAGE = "icd")
}

#' Get or set development mode
#'
#' This just makes it easier to call things like code reformatting, and multiple
#' project working directories.
#' @keywords internal
#' @noRd
icd_devel <- function(devel_mode = NULL) {
  rbi_path <- ihf(".Rbuildignore")
  rbi_devel <- "#^R\\/devel\\.R$"
  rbi_build <- "^R\\/devel\\.R$"
  rbi <- readLines(rbi_path)
  o <- getOption("icd.devel", default = FALSE)
  set_rbi_devel_mode <- function(devel) {
    if (devel) {
      rbi[which(rbi == rbi_build)] <- rbi_devel
    } else {
      rbi[which(rbi == rbi_devel)] <- rbi_build
    }
    writeLines(rbi, con = rbi_path)
  }
  is_rbi_devel <- function() rbi_devel %in% rbi
  is_rbi_build <- function() rbi_build %in% rbi
  if (missing(devel_mode)) {
    message("devel set? opt is: ", o, ", rbi is: ", is_rbi_devel())
    if (o && is_rbi_build()) {
      set_rbi_devel_mode(devel = TRUE)
      warning("Fixed .Rbuildignore - now development mode")
    }
    if (!o && is_rbi_devel()) {
      set_rbi_devel_mode(devel = FALSE)
      warning("Fixed .Rbuildignore - now build/release mode")
    }
    return(o)
  }
  options("icd.devel" = devel_mode)
  set_rbi_devel_mode(devel = devel_mode)
  pkgbuild::check_compiler(TRUE)
  invisible(devel_mode)
}


if (isTRUE(getOption("icd.devel"))) {
  message("attaching dev packages")
  load_dev_pkgs()
  message("sourcing extra test functions")
  source(file.path(ihd(quiet = FALSE), "tools", "extra-tests.R"))

  b <- d <- e <- f <- g <- l <- s <- NA
  class(b) <- "b"
  class(d) <- "d"
  class(e) <- "e"
  class(f) <- "f"
  class(g) <- "g"
  class(l) <- "l"
  class(s) <- "s"
  print.b <- function(...) toolchain()
  print.d <- function(...) icd_devel(TRUE)
  print.e <- function(...) icd_devel(FALSE)
  print.f <- function(...) {
    system2(ihf("tools", "format.sh"), args = ihd())
  }
  print.s <- function(...) {
    styler::style_pkg(filetype = c("R", "Rprofile", "Rmd", "Rnw"))
    owd <- devtools::wd()
    on.exit(setwd(owd), add = TRUE)
    styler::style_dir("tools", filetype = c("R", "r", "Rmd", "rmd"))
    styler::style_dir("benchmarks", filetype = c("R", "r", "Rmd", "rmd"))
    styler::style_dir("benchmarks/icd-JSS3447-replication", filetype = c("R", "r", "Rmd", "rmd"))
  }

  print.g <- function(...) {
    print(git2r::status(ihd()))
  }

  print.l <- function(...) {
    print(lint_extra(ihd()))
    invisible(NULL)
  }
}
