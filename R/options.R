.show_options <- function() {
  o <- options()
  o[grepl("^icd\\.data", names(o))]
}

#' Set initial options for the package
#'
#' \code{icd.data.offline} - default is \code{TRUE}, unless the system
#' environment vairable \code{ICD_DATA_OFFLINE} is \sQuote{false} or
#' \sQuote{no}. This will only ever be turned on with explicit user
#' authorization (or by directly setting it). Turning this on also results in
#' data being saved in the data directory. See below.
#'
#' \code{icd.data.interact} - default is based on interactive mode of R, as
#' given by \code{base::interactive()}, but can be overridden, e.g. to simulate
#' non-interactive testing in an interactive environment.
#'
#' \code{icd.data.resource} - default is ~/.icd.data but won't write unless user
#' gives permission, e.g., using \code{\link{setup_icd_data}}
#'
#' \code{icd.data.absent_action} - what to do if data is missing, \sQuote{stop},
#' \sQuote{warning}, \sQuote{message"}, or \sQuote{silent}.
#'
#' \code{icd.data.icd10cm_active_ver} - which ICD-10-CM version is currently
#' active. Default is \sQuote{2019}.
#'
#' See also \code{.show_options} \code{.clear_options} \code{.set_dev_options}
#' @keywords internal
#' @noRd
.set_init_options <- function() {
  if (!("icd.data.verbose" %in% names(options()))) {
    options("icd.data.verbose" = .env_var_is_true("ICD_DATA_VERBOSE"))
  }
  if (!("icd.data.offline" %in% names(options()))) {
    options("icd.data.offline" = !.env_var_is_false("ICD_DATA_OFFLINE"))
  }
  if (!("icd.data.interact" %in% names(options()))) {
    options(
      "icd.data.interact" =
        .env_var_is_true("ICD_DATA_INTERACT") ||
          interactive()
    )
  }
  # stop or message, anything else will silently continue, which we have to
  # default to onLoad to avoid numerous R CMD check problems. For this reason
  # also, don't check whether option already set, just to make sure we are
  # really silent with CRAN.
  if (!("icd.data.absent_action" %in% names(options()))) {
    ev <- tolower(Sys.getenv("ICD_DATA_ABSENT_ACTION", unset = "stop"))
    stopifnot(ev %in% c(
      "message",
      "stop",
      "warning",
      "silent"
    ))
    options("icd.data.absent_action" = ev)
  }
  # Which version of ICD-10-CM to use by default?
  if (!("icd.data.icd10cm_active_ver" %in% names(options()))) {
    set_icd10cm_active_ver("2019", check_exists = FALSE)
  }
  if (!("icd.data.resource" %in% names(options()))) {
    for (trypath in c(
      getOption("icd.data.resource", default = NA),
      Sys.getenv("ICD_DATA_PATH", unset = NA),
      file.path(Sys.getenv("HOME"), ".icd.data"),
      path.expand(.icd_data_default)
    )) {
      if (!is.na(trypath) && dir.exists(trypath)) {
        if (any(grepl("tmp", trypath))) warning("Using temporary directory.")
        options("icd.data.resource" = trypath)
      }
    }
  }
}

.set <- function(..., overwrite = FALSE) {
  f <- list(...)
  invisible(
    lapply(
      names(f),
      function(o) {
        if (overwrite || is.null(getOption(o))) {
          args <- list(f[[o]])
          names(args) <- paste0("icd.data.", o)
          do.call(options, args = args)
        }
      }
    )
  )
}

.set_hard <- function(...) {
  .set(..., overwrite = TRUE)
}

.set_default_options <- function(hard) {
  f <- if (hard) .set_hard else .set
  f(
    offline = TRUE,
    absent_action = "stop",
    icd10cm_active_ver = "2019",
    resource = .icd_data_default,
    interact = interactive(),
    verbose = TRUE
  )
}

.set_test_options <- function() {
  .set_hard(
    interact = FALSE,
    verbose = TRUE
  )
}

# Simulate the empty world of CRAN and R CMD check
# .set_check_options <- function() {
#   .set_hard(
#     interact = FALSE,
#     absent_action = "silent",
#     verbose = FALSE
#   )
#   if (is.null(icd_data_dir())) {
#     .set(resource = td <- tempdir())
#     message("Created temporary resource directory: ", td)
#   }
# }

.set_dev_options <- function() {
  .set_default_options(hard = TRUE)
  .set(
    offline = FALSE,
    absent_action = TRUE,
    resource = .icd_data_default
  )
}

.verbose <- function(x) {
  if (missing(x)) {
    v <- getOption("icd.data.verbose")
    if (is.numeric(v)) return(v)
    return(isTRUE(v))
  }
  if (is.logical(x) && length(x) == 1L && !is.na(x))
    options(icd.data.verbose = x)
  else
    options("icd.data.verbose" = .env_var_is_true("ICD_DATA_VERBOSE"))
  invisible(getOption("icd.data.verbose"))
}

.interact <- function(x) {
  if (missing(x)) {
    return(isTRUE(getOption("icd.data.interact")))
  }
  if (is.logical(x) && length(x) == 1L && !is.na(x))
    options(icd.data.interact = x)
  else
    options("icd.data.interact" = .env_var_is_true("ICD_DATA_INTERACT"))
  invisible(getOption("icd.data.interact"))
}

.offline <- function(x) {
  if (missing(x)) {
    return(isTRUE(getOption("icd.data.offline")))
  }
  if (is.logical(x) && length(x) == 1L && !is.na(x))
    options(icd.data.offline = x)
  else
    options("icd.data.offline" = !.env_var_is_false("ICD_DATA_OFFLINE"))
  invisible(getOption("icd.data.offline"))
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
    if (is.na(x) || x == "sysenv")
      options("icd.data.absent_action" = Sys.getenv("ICD_DATA_ABSENT_ACTION"))
    else
      options("icd.data.absent_action" = x)
    return(getOption("icd.data.absent_action"))
  }
  a <- getOption("icd.data.absent_action")
  # default to silent, as I think R check uses empty options for various parts of check, which ignore anything I might have wanted to set in .onLoad .
  if (is.null(a)) {
    "silent"
  } else {
    a
  }
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

.clear_options <- function() {
  icd_data_opts <- names(.show_options())
  icd_data_opts <- sapply(
    icd_data_opts,
    simplify = FALSE,
    USE.NAMES = TRUE,
    FUN = function(x) NULL
  )
  options(icd_data_opts)
}

with_offline <- function(offline, code) {
  old <- options("icd.data.offline" = offline)
  on.exit(options(old))
  force(code)
}

with_interact <- function(interact, code) {
  old <- options("icd.data.interact" = interact)
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
  old <- options("icd.data.absent_action" = absent_action)
  on.exit(options(old))
  force(code)
}

#' Set up the data download cache, give permission to download data
#'
#' This must be called by the user, as prompted on package attach with
#' \code{library(icd)}.
#' @param path Path to a directory where cached online raw and parsed data will
#'   be cached. It will be created if it doesn't exist.
#' @examples
#' \dontrun{
#' setup_icd_data()
#' setup_icd_data("/var/cache/icd.data")
#' setup_icd_data(path = ".local/icd.data")
#' icd_data_dir()
#' }
#' @return The path to the resource directory, or \code{NULL} if it could not be
#'   found.
#' @return Invisibly returns the data path which was set, or NULL if not done.
#' @seealso \code{\link{download_icd_data}}
#' @export
setup_icd_data <- function(path = NULL) {
  options("icd.data.offline" = FALSE)
  if (!is.null(path)) {
    message("Using the icd data cache set by argument from user: ", path)
  }
  if (is.null(path)) {
    path <- getOption("icd.data.resource", default = NULL)
    message("Trying the icd data cache set by option(\"icd.data.resource\"): ", path) # nolint
  }
  if (is.null(path)) {
    path <- Sys.getenv("ICD_DATA_RESOURCE", unset = NA)
    message("Trying the icd data cache set by the environment variable ICD_DATA_RESOURCE: ", path) # nolint
    if (is.na(path)) path <- NULL
  }
  if (is.null(path)) {
    path <- .icd_data_default
    message("Trying the default icd data cache: ", path)
  }
  if (is.null(path)) {
    stop("Unable to find a path to use for icd data cache.")
  }
  if (!dir.exists(path)) {
    created <- dir.create(path, showWarnings = TRUE)
    if (!created) stop("Unable to create directory at: ", path)
  }
  options("icd.data.resource" = path)
  invisible(path)
}

#' Download all the additional data at once
#'
#' This may take ten minutes on a broadband connection. It will download and
#' parse WHO ICD-10, French, and Belgian codes and descriptions. It will also
#' get years 2014, 2015, 2017, and 2018 for ICD-10-CM (diagnostic codes), and
#' 2014--2019 procedure codes. 2016 and 2019 diagnostic codes are included in
#' the package data.
#' @seealso \code{\link{setup_icd_data}}
#' @examples
#' \dontrun{
#' setup_icd_data()
#' download_icd_data()
#' }
#' @export
download_icd_data <- function() {
  setup_icd_data()
  message("Downloading, caching and parsing all ICD data")
  message("This will take a few minutes.")
  options("icd.data.offline" = FALSE)
  for (d in .data_names) {
    message("Working on: ", d)
    .get_fetcher_fun(d)()
  }
}
