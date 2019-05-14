.opt_names <- c(
  "verbose",
  "resource",
  "icd10cm_active_year",
  "offline",
  "test_slow",
  "interact"
)

.opt_full_name <- function(opt_name) {
  paste0("icd.", opt_name)
}

.show_options <- function() {
  o <- options()
  o[grepl("^icd\\.", names(o))]
}

.print_options <- function() {
  cat(
    paste(
      names(.show_options()),
      .show_options(),
      sep = "=",
      collapse = ", "
    ),
    fill = TRUE
  )
}

#' Set initial options for the package
#'
#' \code{icd.offline} - default is \code{TRUE}, unless the system
#' environment vairable \code{ICD_DATA_OFFLINE} is \sQuote{false} or
#' \sQuote{no}. This will only ever be turned on with explicit user
#' authorization (or by directly setting it). Turning this on also results in
#' data being saved in the data directory. See below.
#'
#' \code{icd.interact} - default is based on interactive mode of R, as
#' given by \code{base::interactive()}, but can be overridden, e.g. to simulate
#' non-interactive testing in an interactive environment.
#'
#' \code{icd.resource} - default is ~/.icd.data but won't write unless user
#' gives permission, e.g., using \code{\link{set_icd_data_dir}}
#'
#' \code{icd.absent_action} - what to do if data is missing, \sQuote{stop},
#' \sQuote{warning}, \sQuote{message"}, or \sQuote{silent}.
#'
#' \code{icd.icd10cm_active_year} - which ICD-10-CM version is currently
#' active. Default is \sQuote{2019}.
#'
#' See also \code{.show_options} \code{.clear_options}
#' @keywords internal
#' @noRd
NULL

# the whole point of this is to have 'enum' like behavior, so I can't mistype an
# option name string elsewhere.
.set_opt <- function(...) {
  f <- list(...)
  stopifnot(!any(grepl(names(f), pattern = "^icd\\.")))
  names(f) <- paste0("icd.", names(f))
  options(f)
}

# only get options we know about, to avoid typo giving a NULL
.get_opt <- function(x, default = NULL) {
  o <- as.character(substitute(x))
  getOption(.opt_full_name(o), default = default)
}

.verbose <- function(x) {
  if (missing(x)) {
    v <- .get_opt("verbose")
    if (is.numeric(v)) {
      return(as.integer(v))
    }
    return(isTRUE(v))
  }
  if ((is.logical(x) || is.numeric(x)) &&
    length(x) == 1L &&
    !is.na(x)) {
    if (is.numeric(x)) x <- as.integer(x)
    .set_opt("verbose" = x)
  } else {
    ev <- .env_var_is_true("ICD_DATA_VERBOSE")
    .set_opt("verbose" = ev)
    if (ev) message("Reset verbose option to ICD_DATA_VERBOSE")
  }
  gcinfo(x > 5)
  invisible(.get_opt("verbose"))
}

.interact <- function(x) {
  if (missing(x)) {
    if (is.na(.get_opt("interact", default = NA)) &&
      !is.na(Sys.getenv("ICD_DATA_INTERACT", unset = NA))) {
      .msg("Setting interactivity with env var")
      .set_opt("interact" = .env_var_is_true("ICD_DATA_INTERACT"))
    }
    opt <- .get_opt("interact", default = NA)
    if (is.na(opt)) {
      .msg("interact option not set, so falling back on interactive()")
      .set_opt("interact" = interactive())
    }
    stopifnot(is.logical(opt))
    return(opt)
  }
  if (is.logical(x) && length(x) == 1L && !is.na(x)) {
    .set_opt("interact" = x)
  } else {
    stop(".interact() requires a single logical value, or a missing value.")
  }
  invisible(.get_opt("interact"))
}

.offline <- function(x) {
  if (missing(x)) {
    return(isTRUE(.get_opt("offline")))
  }
  if (is.logical(x) && length(x) == 1L && !is.na(x)) {
    .set_opt("offline" = x)
  } else {
    stop("offline() requires a single logical value, or a missing value.")
  }
  invisible(.get_opt("offline"))
}

.test_slow <- function(x) {
  if (missing(x)) {
    return(.get_opt("test_slow", default = FALSE))
  }
  stopifnot(is.logical(x) && length(x) == 1)
  Sys.setenv("ICD_TEST_SLOW" = x)
  .set_opt("test_slow" = x)
  invisible(x)
}

.absent_action <- function(x = c(
                             "stop",
                             "warning",
                             "message",
                             "silent",
                             "sysenv",
                             NA
                           )) {
  if (!missing(x)) {
    x <- match.arg(x)
    if (is.na(x) || x == "sysenv") {
      .set_opt("absent_action" = Sys.getenv("ICD_DATA_ABSENT_ACTION"))
    } else {
      .set_opt("absent_action" = x)
    }
    return(.get_opt("absent_action"))
  }
  # default stop instead of silent now not using active bindings?
  .get_opt("absent_action", default = "stop")
}

.absent_action_switch <- function(msg, must_work = TRUE) {
  switch(.absent_action(),
    "stop" = {
      if (must_work) {
        stop(msg, call. = FALSE)
      } else {
        message(msg, call. = FALSE)
      }
    },
    "warning" = {
      if (must_work) {
        warning(msg, call. = FALSE)
      } else {
        message(msg, call. = FALSE)
      }
    },
    "message" = message(msg)
  )
  invisible()
}

.env_var_is_false <- function(x) {
  ev <- Sys.getenv(x, unset = "")
  tolower(ev) %in% c(
    "n",
    "no",
    "false",
    "0"
  )
}

.env_var_is_true <- function(x) {
  ev <- Sys.getenv(x, unset = "")
  tolower(ev) %in% c(
    "y",
    "yes",
    "true",
    "1"
  )
}

with_offline <- function(offline, code) {
  old <- .set_opt("offline" = offline)
  on.exit(options(old))
  force(code)
}

with_interact <- function(interact, code) {
  old <- .set_opt("interact" = interact)
  on.exit(options(old))
  force(code)
}

with_absent_action <- function(absent_action = c(
                                 "message",
                                 "stop",
                                 "warning",
                                 "silent"
                               ),
                               code) {
  absent_action <- match.arg(absent_action)
  old <- .set_opt("absent_action" = absent_action)
  on.exit(options(old))
  force(code)
}

#' Set up the data download cache, give permission to download data
#'
#' This must be called by the user, as prompted on package attach with
#' \code{library(icd)}.
#' @param path Path to a directory where cached online raw and parsed data will
#'   be cached. It will be created if it doesn't exist.
#' @param must_work Logical, the default of \code{TRUE} will cause this to stop
#'   with an error if a usable icd data directory cannot be found or set.
#' @examples
#' \dontrun{
#' set_icd_data_dir()
#' # or choose another directory:
#' # set_icd_data_dir("/var/cache/icd.data")
#' # then you may use:
#' # download_all_icd_data()
#' # or let 'icd' download data when needed.
#' }
#' @return The path to the resource directory, or \code{NULL} if it could not be
#'   found.
#' @return Invisibly returns the data path which was set, or NULL if not done.
#' @seealso \code{\link{download_all_icd_data}}
#' @export
set_icd_data_dir <- function(path = NULL) {
  .set_opt("offline" = FALSE)
  if (!is.null(path)) {
    .msg("Using the icd data cache set by argument from user: ", path)
  }
  if (is.null(path)) {
    path <- .get_opt("resource", default = NULL)
    .msg("Trying the icd data cache set by option(\"icd.data.resource\"): ", path) # nolint
  }
  if (is.null(path)) {
    path <- Sys.getenv("ICD_DATA_RESOURCE", unset = NA)
    .msg("Trying the icd data cache set by the environment variable ICD_DATA_RESOURCE: ", path) # nolint
    if (is.na(path)) path <- NULL
  }
  if (is.null(path)) {
    path <- .default_icd_data_dir()
    .msg("Trying the default icd data cache: ", path)
  }
  if (is.null(path)) {
    stop(
      "Unable to find a path to use for icd data cache. Try ",
      sQuote("set_icd_data_dir(\"/path/with/write/access\")")
    )
  }
  if (!dir.exists(path)) {
    created <- dir.create(path, showWarnings = TRUE)
    if (!created) stop("Unable to create directory at: ", path)
  }
  .set_opt("resource" = path)
  if (!.all_cached() && "download_all_icd_data" %nin% names(sys.calls())) {
    message(
      "Not all available data is currently downloaded. ",
      "You may use: ", sQuote("download_all_icd_data()"),
      " to complete downloading all available data, or let this happen on demand."
    )
  }
  message("Using ", sQuote(path), " for saving ICD data.")
  invisible(path)
}

#' Download all the additional data at once
#'
#' It will download and parse WHO ICD-10, French, and Belgian codes and
#' descriptions. It will also get years 2014, 2015, 2017, and 2018 for ICD-10-CM
#' (diagnostic codes), and 2014--2019 procedure codes. 2016 and 2019 diagnostic
#' codes are included in the package data. The total amount of data is about
#' 340Mb. It is not necessary to do call \code{download_all_icd_data} for normal
#' use: you may simply call the functions like \code{get_icd10cm2014}, which
#' will download data when needed.
#' @seealso \code{\link{set_icd_data_dir}}
#' @examples
#' \dontrun{
#' set_icd_data_dir()
#' # or configure a directory to us:
#' # .set_opt("resource" = "/tmp/icd")
#' # or
#' # set_icd_data_dir("/tmp/icd")
#'
#' # The following would download, and make all the known ICD data available
#' # download_all_icd_data()
#' }
#' @export
download_all_icd_data <- function() {
  set_icd_data_dir()
  message("Downloading, caching and parsing all ICD data")
  message("This will take a few minutes, and use about 340MB.")
  .set_opt("offline" = FALSE)
  for (d in .data_names) {
    message("Working on: ", d)
    .get_fetcher_fun(d)()
  }
}
