#' Get or check existence of a getter function, returning the name of, or the
#' function itself
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
#' @seealso \code{\link{.get}}, \code{.get_getter_name} and
#'   \code{.get_parser_icd10cm_name}
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

.get_icd9cm_name <- function(year, leaf) {
  paste0("icd9cm", year, ifelse(leaf, "_leaf", ""))
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
  stopifnot(length(var_name) == 1L)
  match.fun(.get_fetcher_name(var_name))
}

.exists_in_cache <- function(var_name, ...) {
  vapply(var_name, .exists_in_cache_single, logical(1), ...)
}

.exists_in_cache_single <- function(var_name) {
  stopifnot(length(var_name) == 1L)
  .dbg("Seeing if ", sQuote(var_name), " exists in cache env or dir")
  if (!.exists_icd_data_dir()) {
    .msg(
      "Don't even have the icd.cache option defined,",
      " and default location of ", sQuote(.default_icd_data_dir()),
      " is missing."
    )
    return(FALSE)
  }
  stopifnot(is.character(var_name))
  .msg(".exists_in_cache trying icd_data_env environment")
  if (.exists(var_name)) {
    .msg(sQuote(var_name), " found in env.")
    return(TRUE)
  }
  fp <- .rds_path(var_name)
  .msg("Checking if we have file path for exists")
  if (is.null(fp)) {
    return(FALSE)
  }
  .msg("Trying file at: ", fp)
  return(file.exists(fp))
  .msg(var_name, " not seen in cache env or dir.")
  FALSE
}

.get_from_cache <- function(var_name,
                            must_work = TRUE) {
  verbose <- .verbose() > 1
  if (verbose) {
    message(
      "Trying to get ", sQuote(var_name), " from cache env or dir"
    )
  }
  .msg(".get_from_cache Trying icd_data_env environment")
  if (.exists(var_name)) {
    .msg("Found in env cache")
    return(.get(var_name))
  }
  if (!.exists_in_cache(var_name = var_name)) {
    msg <- paste("Unable to get cached data for:", sQuote(var_name))
    .absent_action_switch(msg, must_work = must_work)
    return(invisible())
  }
  fp <- .rds_path(var_name)
  .msg("Checking if we have file path for get")
  if (is.null(fp)) {
    return()
  }
  .msg("Getting file at: ", fp)
  val <- readRDS(fp)
  .assign(var_name, val)
  val
}

.all_cached <- function() {
  if (is.null(.get_opt("cache"))) {
    return(FALSE)
  }
  vec <- vapply(.data_names_cache, .exists_in_cache, logical(1))
  res <- all(vec)
  if (!res) {
    .dbg("missing data is: ", paste(.data_names[!vec], collapse = ", "))
  }
  res
}

.make_getter <- function(var_name) {
  force(var_name)
  getter_fun <- function(alt = NULL,
                           must_work = TRUE,
                           msg = paste("Unable to find", var_name)) {
    verbose <- .verbose()
    .msg("Starting getter for: ", var_name)
    stopifnot(is.character(var_name))
    dat <- .get_from_cache(var_name,
      must_work = FALSE
    )
    if (!is.null(dat)) {
      .msg("Found in cache ", var_name, " in cache.")
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
  # Fetcher function (this is the get_ exported in NAMESPACE for the user)
  fetcher_fun <- function(alt = NULL,
                            must_work = is.null(alt),
                            msg = paste("Unable to find", var_name)) {
    verbose <- .verbose()
    .msg("Starting fetcher for ", var_name)
    if (.exists_anywhere(var_name)) {
      .msg("Found ", var_name, " in cache or package data.")
      return(.get_anywhere(var_name = var_name))
    }
    .msg("Not in cache or package data")
    if (!.icd_data_dir_okay() && .offline()) {
      .msg("Offline and/or no cache")
      .absent_action_switch(
        "Offline so not attempting to download or parse",
        must_work = must_work
      )
      return(alt)
    }
    .msg("Trying again to get from anywhere - unnecessary?")
    # duplicated from above:
    if (.exists_anywhere(var_name)) {
      .msg("Found ", var_name, " in cache or package data.")
      return(.get_anywhere(var_name = var_name))
    }
    if (verbose) {
      message(
        "Trying to find parse function: ",
        sQuote(parse_fun_name)
      )
    }
    fr <- environment()
    if (exists(parse_fun_name, fr, inherits = TRUE)) {
      .msg("Found parse function. Calling it.")
      out <- do.call(
        get(parse_fun_name,
          envir = fr,
          inherits = TRUE
        ),
        args = list()
      )
      if (verbose && is.null(out)) message("Returning NULL")
      if (!is.null(out) && !.offline()) {
        .save_in_cache(x = out, var_name = var_name)
      }
      return(out)
    } else {
      stop("No parse function: ", parse_fun_name)
    }
    .msg("Getting form cache now we have parsed.")
    # Parse function should have saved the data in env and file caches
    dat <- .get_from_cache(var_name, must_work = FALSE)
    if (!is.null(dat)) {
      return(dat)
    }
    if (must_work) {
      stop(
        "Cannot fetch (download/parse/get from cache) that data using ",
        parse_fun_name, ", and it must work."
      )
    }
    if (is.null(alt)) {
      .msg("Returning NULL, as alternative data are not set")
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
  for (var_name in .data_names) {
    .msg("Making getters and fetchers for ", var_name)
    getter_name <- .get_getter_name(var_name)
    .msg("assigning: ", getter_name)
    # TODO: this doesn't need to be specific to each data element?
    assign(getter_name,
      .make_getter(var_name),
      envir = final_env
    )
    fetcher_name <- .get_fetcher_name(var_name)
    .msg("assigning: ", fetcher_name)
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
                   ...) {
  if (.exists_in_cache(var_name)) {
    .get_from_cache(var_name,
      must_work = TRUE
    )
  } else {
    .get_parser_fun(var_name)()
  }
}

.exists_icd_data_dir <- function() {
  path <- .get_opt("cache")
  !is.null(path) && dir.exists(path)
}

.set_icd_data_dir <- function(path) {
  .dbg("icd_data_dir: options are currently:")
  if (.verbose() > 1) .print_options()
  if (missing(path)) path <- .default_icd_data_dir()
  if (!dir.exists(path)) {
    if (!dir.create(path, recursive = TRUE, showWarnings = FALSE)) {
      stop("Could not create directory at: ", sQuote(path))
    }
  }
  if (file.access(path, 2) != 0) {
    stop("icd default data path ", sQuote(path), " is not writable")
  }
  .set_opt("cache" = path)
  invisible(path)
}

#' Get a default data directory for any platform
#' @examples
#' # Consider to allow versioned data directories
#' utils::packageVersion("icd")
#' @keywords internal
#' @noRd
.default_icd_data_dir <- function() {
  rappdirs::user_data_dir(appname = "icd")
}

#' @describeIn set_icd_data_dir Get the currently active data directory, and
#'   check it exists and is writable.
#' @export
get_icd_data_dir <- function(must_work = TRUE) {
  get_started <- paste(
    "Use set_icd_data_dir() to get started. ",
    "(The", sQuote("icd.cache"),
    "option is not set, and the default OS-dependent icd data directory",
    "does not exist yet. You may also use ",
    sQuote("set_icd_data_dir(\"/path/of/your/choice\")"), "."
  )
  o <- .get_opt("cache")
  if (!is.null(o)) {
    .dbg("icd.cache option set to: ", o, ", so using it.")
    if (!.dir_writable(o)) {
      msg <- paste(
        "icd.cache option set to:", o,
        "but the location is not writable or doesn't exist.",
        get_started
      )
      if (must_work) {
        stop(msg)
      } else {
        .msg(msg)
        return(NA_character_)
      }
    }
  } else {
    o <- .default_icd_data_dir()
    if (dir.exists(o) && .dir_writable(o)) {
      .dbg(
        "icd.cache option is not set, but default path: ",
        sQuote(o), " exists, so using it and setting option."
      )
      .set_opt("cache" = o)
    } else {
      msg <- paste(
        "icd.cache not set and default location",
        sQuote(o), "is not writable or doesn't exist.",
        get_started
      )
      if (must_work) {
        stop(msg)
      } else {
        .msg(msg)
        return(NA_character_)
      }
    }
  }
  o
}

.icd_data_dir_okay <- function() {
  dir <- with_absent_action(
    absent_action = "silent",
    get_icd_data_dir(must_work = FALSE)
  )

  !is.null(dir) && !is.na(dir) && .dir_writable(dir)
}

.clean <- function(env = TRUE,
                   opts = FALSE,
                   rds = FALSE,
                   memoise = FALSE,
                   raw = FALSE,
                   destroy = FALSE,
                   pattern = ".*",
                   dry_run = FALSE) {
  pattern <- paste0(".*", pattern, ".*")
  if (env) {
    if (!dry_run) {
      rm(
        list = ls(.icd_data_env,
          all.names = TRUE,
          pattern = pattern
        ),
        envir = .icd_data_env
      )
    }
  }
  if (opts) {
    icd_data_opts <- names(.show_options())
    icd_data_opts <- sapply(
      icd_data_opts,
      simplify = FALSE,
      USE.NAMES = TRUE,
      FUN = function(x) NULL
    )
    options(icd_data_opts)
  }
  if (destroy) {
    if (askYesNo("Destroy entire cache directory? (Consider .hide_resource_dir() instead?)")) {
      if (!dry_run) {
        unlink(get_icd_data_dir(), recursive = TRUE)
      }
    }
    return(invisible())
  }
  if (memoise) {
    message("deleting memoise directory")
    if (!dry_run) {
      unlink(
        file.path(get_icd_data_dir(), "memoise"),
        recursive = TRUE
      )
    } else {
      message("dry run")
    }
  }
  if (raw) {
    raw_files <- list.files(get_icd_data_dir(),
      pattern = sprintf(
        fmt = "(%s\\.txt$)|(%s\\.xlsx$)",
        pattern, pattern
      ),
      ignore.case = TRUE,
      full.names = TRUE
    )
    message("Deleting:")
    print(raw_files)
    if (!dry_run) {
      unlink(raw_files, recursive = FALSE)
    } else {
      message("dry run")
    }
  }
  if (rds) {
    rds_files <- list.files(get_icd_data_dir(),
      paste0(pattern, "\\.rds"),
      full.names = TRUE
    )
    message("Deleting:")
    print(rds_files)
    if (!dry_run) {
      unlink(rds_files,
        recursive = FALSE
      )
    } else {
      message("dry run")
    }
  }
}

.confirm_download <- function(msg = NULL) {
  if (!.offline()) {
    if (!.exists_icd_data_dir()) {
      set_icd_data_dir()
    }
    return(TRUE)
  }
  ok <- FALSE
  if (.interact()) {
    message(
      "icd needs to download and/or parse data.",
      "It will be saved in an OS-specific data directory, ",
      "or according to the R option: ", sQuote("icd.cache")
    )
    if (.verbose() && !is.null(msg)) message(msg)
    ok <- isTRUE(
      askYesNo(
        "May I download and cache a few MB per ICD edition as needed?"
      )
    )
  }
  .set_opt("offline" = !ok)
  if (!ok) .absent_action_switch("Unable to get permission to download data.")
  ok
}

.rds_path <- function(var_name) {
  fp <- file.path(get_icd_data_dir(), paste0(var_name, ".rds"))
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
  vapply(var_name, exists, envir = .icd_data_env, logical(1))
}

.get <- function(var_name) {
  stopifnot(length(var_name) == 1)
  get(x = var_name, envir = .icd_data_env)
}

.assign <- function(var_name, value) {
  stopifnot(length(var_name) == 1)
  assign(
    x = var_name,
    value = value,
    envir = .icd_data_env
  )
}

.ls <- function() {
  ls(.icd_data_env, all.names = TRUE)
}

.ls_cache <- function(...) {
  list.files(path = get_icd_data_dir(), ...)
}

# for development, list envrionment of a function
.ls_fun <- function(f) ls(environment(f))

#' List the actual data in this package, not bindings
#' @examples
#' icd:::.ls_icd_data()
#' @keywords datasets internal
#' @noRd
.ls_icd_data <- function() {
  utils::data(package = "icd")$results[, "Item"]
}

.hide_cache_dir <- function() {
  dir <- get_icd_data_dir(must_work = FALSE)
  if (is.na(dir)) dir <- .default_icd_data_dir()
  hidden_dir <- paste0(dir, ".hidden")
  if (dir.exists(dir) && !dir.exists(hidden_dir)) {
    file.rename(dir, hidden_dir)
    message("hidden")
  } else if (dir.exists(hidden_dir) && !dir.exists(dir)) {
    file.rename(hidden_dir, dir)
    message("replaced")
  } else {
    stop("neither or both directories exist")
  }
}

.get_lazy <- function(var_name) {
  stopifnot(length(var_name) == 1)
  ns <- asNamespace("icd")
  lz <- ns$.__NAMESPACE__.$lazydata
  get(var_name, lz)
}

.exists_in_lazy <- function(var_name) {
  ns <- asNamespace("icd")
  lz <- ns$.__NAMESPACE__.$lazydata
  vapply(var_name,
    exists,
    FUN.VALUE = logical(1),
    lz
  )
}

.get_anywhere <- function(var_name, fetch = FALSE) {
  if (.verbose()) {
    message(".get_anywhere: ", var_name)
    if (.exists_in_lazy(var_name)) message("lazy")
    if (.exists_in_cache(var_name)) message("cache")
    ns <- asNamespace("icd")
    if (exists(var_name, ns)) message("from package namespace itself")
    if (fetch) message("will try to fetch") else message("not going to fetch")
  }
  if (.exists_in_lazy(var_name)) {
    return(.get_lazy(var_name))
  }
  if (.exists_in_cache(var_name)) {
    return(.get_from_cache(var_name))
  }
  ns <- asNamespace("icd")
  if (exists(var_name, ns)) {
    return(get(var_name, ns))
  }
  if (fetch && exists(.get_fetcher_name(var_name), ns, mode = "function")) {
    return(.get_fetcher_fun(var_name)())
  }
  .absent_action_switch(
    paste(var_name, "not available in icd.data regular or lazy data")
  )
}

.exists_anywhere <- function(var_name, fetch = FALSE) {
  ns <- asNamespace("icd")
  vapply(var_name, function(v) {
    r <- .exists_in_lazy(v) ||
      .exists_in_cache(v) ||
      exists(v, ns)
    if (!r) {
      if (fetch && exists(.get_fetcher_name(v), ns, mode = "function")) {
        dat <- with_absent_action("silent", .get_fetcher_fun(v))
        if (!is.null(dat) && is.data.frame(dat)) r <- TRUE
      }
    }
    r
  },
  FUN.VALUE = logical(1)
  )
}
