#' Get or check existence of a getter function, returning the name of, or the function itself
#'
#' The getter looks first in the environment cache, then file cache for a parsed
#' \code{.rds} file. It does not try to download or parse data. For that, see
#' \code{\link{.get_parser_name}}
#' @keywords internal
#' @noRd
.get_getter_name <- function(var_name) {
  paste0(".get_", paste0(var_name))
}

.get_getter_fun <- function(var_name) {
  match.fun(.get_getter_name(var_name))
}

#' Parse ICD data (downloading data if needed)
#' @seealso \code{\link{.get}}, \code{.get_getter_name} and \code{.get_parser_icd10cm_name}
#' @keywords internal
#' @noRd
.get_parser_name <- function(var_name) {
  paste0(".parse_", var_name)
}

.get_parser_fun <- function(var_name) {
  match.fun(.get_parser_name(var_name))
}

.get_parser_icd9cm_leaf_name <- function(ver) {
  paste0(".parse_", paste0("icd9cm", ver, "_leaf"))
}

.get_parser_icd9cm_rtf_name <- function(ver) {
  paste0(".parse_", paste0("icd9cm", ver))
}

.get_icd10cm_name <- function(year, dx) {
  paste0("icd10cm", year, ifelse(dx, "", "_pc"))
}

.get_parser_icd10cm_name <- function(year, dx) {
  paste0(".parse_", .get_icd10cm_name(year, dx))
}

#' Get name or function for fetching a specific ICD data set
#' @seealso \code{\link{.fetch}}
#' @keywords internal
#' @noRd
.get_fetcher_name <- function(var_name) {
  paste0("get_", var_name)
}

.get_fetcher_fun <- function(var_name) {
  match.fun(.get_fetcher_name(var_name))
}

.exists_in_cache <- function(var_name, verbose = .verbose()) {
  if (verbose > 1) {
    message("Seeing if ", sQuote(var_name), " exists in cache env or dir")
  }
  if (is.null(getOption("icd.data.resource", default = NULL))) {
    message("Don't even have the icd.data.resource option defined.")
    return(FALSE)
  }
  stopifnot(is.character(var_name))
  if (verbose > 1) message(".exists_in_cache trying icd_data_env environment")
  if (.exists(var_name)) {
    if (verbose) message(sQuote(var_name), " found in cache.")
    return(TRUE)
  }
  fp <- .rds_path(var_name)
  if (verbose > 1) message("Checking if we have file path for exists")
  if (is.null(fp)) return(FALSE)
  if (verbose > 1) message("Trying file at: ", fp)
  return(file.exists(fp))
  if (verbose > 1) message(var_name, " not seen in cache env or dir.")
  FALSE
}

.get_from_cache <- function(var_name,
                            must_work = TRUE) {
  verbose <- .verbose()
  if (verbose) {
    message(
      "Trying to get ", sQuote(var_name), " from cache env or dir"
    )
  }
  if (verbose) message(".get_from_cache Trying icd_data_env environment")
  if (.exists(var_name)) {
    if (verbose) message("Found in env cache")
    return(.get(var_name))
  }
  if (!.exists_in_cache(var_name = var_name, verbose = verbose)) {
    msg <- paste("Unable to get cached data for:", var_name)
    .absent_action_switch(msg, must_work = must_work)
    return()
  }
  fp <- .rds_path(var_name)
  if (verbose) message("Checking if we have file path for get")
  if (is.null(fp)) return()
  if (verbose) message("Getting file at: ", fp)
  val <- readRDS(fp)
  .assign(var_name, val)
  val
}

.all_cached <- function() {
  if (is.null(getOption("icd.data.resource", default = NULL))) return(FALSE)
  all(
    vapply(.data_names, .exists_in_cache, logical(1))
  )
}

.clean_env <- function() {
  rm(list = ls(.icd_data_env, all.names = TRUE), envir = .icd_data_env)
}

.clean_resource_dir <- function(rds = FALSE,
                                memoise = FALSE,
                                raw = FALSE,
                                destroy = FALSE) {
  if (destroy) {
    askYesNo("Destroy entire resource directory?")
    unlink(icd_data_dir(), recursive = TRUE)
    return(invisible())
  }
  if (memoise) {
    message("deleting memoise directory")
    unlink(
      file.path(icd_data_dir(), "memoise"),
      recursive = TRUE
    )
  }
  if (raw) {
    raw_files <- list.files(icd_data_dir(),
      pattern = "(\\.txt$)|(\\.xlsx$)",
      ignore.case = TRUE,
      full.names = TRUE
    )
    message("Deleting:")
    print(raw_files)
    unlink(raw_files, recursive = FALSE)
  }
  if (rds) {
    rds_files <- list.files(icd_data_dir(), ".*\\.rds", full.names = TRUE)
    message("Deleting:")
    print(rds_files)
    unlink(rds_files,
      recursive = FALSE
    )
  }
}

.make_getter <- function(var_name) {
  force(var_name)
  getter_fun <- function(alt = NULL,
                           must_work = TRUE,
                           msg = paste("Unable to find", var_name)) {
    verbose <- .verbose()
    if (verbose) message("Starting getter for: ", var_name)
    stopifnot(is.character(var_name))
    dat <- .get_from_cache(var_name,
      must_work = FALSE
    )
    if (!is.null(dat)) {
      if (verbose) message("Found in cache ", var_name, " in cache.")
      return(dat)
    }
    if (must_work) {
      stop("Cannot get ", sQuote(var_name), " from caches and it must work.")
    }
    if (is.null(alt)) {
      if (verbose) {
        message(
          "Returning NULL as alternative data are not specified for ",
          var_name
        )
      }
      return()
    }
    if (verbose) {
      message("Returning 'alt' as ", var_name, " not available")
    }
    alt
  }
  f_env <- environment(getter_fun)
  f_env$var_name <- var_name
  getter_fun
}

.make_fetcher <- function(var_name) {
  force(var_name)
  parse_fun_name <- .get_parser_name(var_name)
  fetcher_fun <- function(alt = NULL,
                            must_work = is.null(alt),
                            msg = paste("Unable to find", var_name)) {
    verbose <- .verbose()
    if (verbose) message("Starting fetcher for ", var_name)
    dat <- .get_from_cache(
      var_name = var_name,
      must_work = FALSE
    )
    if (!is.null(dat)) {
      if (verbose) message("Found ", var_name, " in cache.")
      return(dat)
    }
    # strictly speaking, we could be offline and already have the raw data, but this is a weird corner case:
    if (.offline()) {
      if (verbose) message("Offline and not in cache")
      .absent_action_switch(
        "Offline so not attempting to download or parse",
        must_work = must_work
      )
      return(alt)
    }
    if (verbose) {
      message(
        "Trying to find parse function: ",
        sQuote(parse_fun_name)
      )
    }
    fr <- environment()
    if (exists(parse_fun_name, fr, inherits = TRUE)) {
      if (verbose) message("Found parse function. Calling it.")
      out <- do.call(
        get(parse_fun_name,
          envir = fr,
          inherits = TRUE
        ),
        args = list()
      )
      if (verbose && is.null(out)) message("Returning NULL")
      if (!is.null(out) && !.offline()) {
        .save_in_resource_dir(x = out, var_name = var_name)
      }
      return(out)
    } else {
      stop("No parse function: ", parse_fun_name)
    }
    # Parse function should have saved the data in env and file caches
    dat <- .get_from_cache(var_name, must_work = FALSE)
    if (!is.null(dat)) return(dat)
    if (must_work) {
      stop(
        "Cannot fetch (download/parse/get from cache) that data using ",
        parse_fun_name, ", and it must work."
      )
    }
    if (is.null(alt)) {
      if (verbose) message("Returning NULL, as alternative data are not set")
      return()
    }
    if (verbose) {
      message(
        "Returning alternative data because ", parse_fun_name,
        " not available"
      )
    }
    alt
  }
  f_env <- environment(fetcher_fun)
  f_env$parse_fun_name <- parse_fun_name
  f_env$var_name <- var_name
  fetcher_fun
}

# called in zzz.R
.make_getters_and_fetchers <- function(final_env = parent.frame()) {
  verbose <- .verbose()
  for (var_name in .data_names) {
    if (verbose) message("Making getters and fetchers for ", var_name)
    getter_name <- .get_getter_name(var_name)
    if (verbose) message("assigning: ", getter_name)
    # TODO: this doesn't need to be specific to each data element?
    assign(getter_name,
      .make_getter(var_name),
      envir = final_env
    )
    fetcher_name <- .get_fetcher_name(var_name)
    if (verbose) message("assigning: ", fetcher_name)
    assign(fetcher_name,
      .make_fetcher(var_name),
      envir = final_env
    )
  }
}

#' Gets data, from env cache, file cache, the downloading and parsing if
#' necessary and possible.
#'
#' Fetch functions are of the form \code{.fetch_icd10cm2015}. This is generic
#' for all datasets, and calls the data specific \code{parse} function, which
#' itself will call (if necessary) the data-specific \code{dl} download
#' function.
#' @param var_name Character, e.g., \code{"icd10cm2015"}
#' @param must_work Single logical
#' @template verbose
#' @param ... E.g., \code{dx = FALSE} or \code{offline = TRUE}
#' @keywords internal
#' @noRd
.fetch <- function(var_name,
                   must_work = TRUE,
                   verbose = .verbose(),
                   ...) {
  if (.exists_in_cache(var_name, verbose = verbose)) {
    .get_from_cache(var_name,
      must_work = TRUE
    )
  } else {
    parser <- .get_parser_fun(var_name)
    parser(
      verbose = verbose,
      # must_work = must_work,
      ...
    )
  }
}

.available <- function(var_name, ...) {
  with_offline(offline = TRUE, {
    !is.null(
      .fetch(
        var_name = var_name,
        must_work = FALSE,
        verbose = .verbose(),
        ...
      )
    )
  })
}

.icd_data_default <- file.path("~", ".icd.data")

.set_icd_data_dir <- function(path = .icd_data_default) {
  if (is.null(path) || !dir.exists(path)) {
    if (!dir.create(path)) stop("Could not create directory at: ", path)
  }
  .set_hard(icd.data.resource = path)
  invisible(path)
}

#' @describeIn setup_icd_data Return the currently active data directory. If
#'   missing, it will return \code{NULL} and, depending on
#'   \code{getOption("icd.data.absent_action")}, will stop, give a message, or
#'   do nothing.
#' @export
icd_data_dir <- function(path) {
  if (!missing(path) && dir.exists(path)) {
    options("icd.data.resource" = path)
    return(path)
  }
  o <- getOption("icd.data.resource", default = NULL)
  if (!is.null(o)) {
    if (any(grepl("tmp", o))) warning("Using a temporary directory")
    return(o)
  }
  if (.verbose()) print(.show_options())
  stop(paste(
    "The", sQuote("icd.data.resource"),
    "option is not set. Use setup_icd_data() to get started."
  ))
}

.confirm_download <- function(absent_action = .absent_action(),
                              interact = .interact(),
                              msg = NULL) {
  if (!.offline()) return(TRUE)
  ok <- FALSE
  if (interact) {
    message("icd needs to download and/or parse data.")
    if (!is.null(msg)) message(msg)
    ok <- isTRUE(
      askYesNo(
        "May I download and cache a few MB per ICD edition as needed?"
      ) # nolint
    )
  }
  options("icd.data.offline" = !ok)
  msg <- "Unable to get permission to download data."
  if (!ok) .absent_action_switch(msg)
  ok
}

.rds_path <- function(var_name) {
  fp <- file.path(icd_data_dir(), paste0(var_name, ".rds"))
  if (length(fp) == 0) {
    NULL
  } else {
    fp
  }
}

#' Check or get data from environment, not file cache
#'
#' @seealso \code{\link{.get_getter_name}} .assign and .ls
#' @keywords internal
#' @noRd
.exists <- function(var_name) {
  exists(x = var_name, envir = .icd_data_env)
}

.get <- function(var_name) {
  get(x = var_name, envir = .icd_data_env)
}

.assign <- function(var_name, value) {
  assign(
    x = var_name,
    value = value,
    envir = .icd_data_env
  )
}

.ls <- function() {
  ls(.icd_data_env, all.names = TRUE)
}

# for development, list envrionment of a function
.ls_fun <- function(f) ls(environment(f))

# for development, list envrionment of a function
.ls.str_fun <- function(f) utils::ls.str(environment(f))

#' List the actual data in this package, not bindings
#' @examples
#' icd:::.ls_icd_data()
#' @keywords datasets internal
#' @noRd
.ls_icd_data <- function() {
  utils::data(package = "icd")$results[, "Item"]
}
