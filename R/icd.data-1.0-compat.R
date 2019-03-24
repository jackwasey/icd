icd_data_ver_ok <- function() {
  # Bug in R? Version of an already lodaded namespace is not checked if done via
  # requireNamespace, only loadNamespace.
  res <- requireNamespace("icd.data", quietly = TRUE)
  res <- res && !getNamespaceVersion(
    asNamespace("icd.data")
  ) <
    as.package_version("1.1")
  res
}

icd_data_icd10cm_active <- function() {
  if (!requireNamespace("icd.data")) {
    message("icd.data doesn't appear to be available")
    return()
  }
  if (!icd_data_ver_ok()) {
    icd.data::icd10cm2016
  } else {
    get("get_icd10cm_active", envir = asNamespace("icd.data"))()
  }
}

icd_data_icd9cm_leaf_v32 <- function() {
  if (!requireNamespace("icd.data")) {
    message("icd.data doesn't appear to be available")
    return()
  }
  icd.data::icd9cm_billable[["32"]]
}

icd_data_get_icd10cm_active_ver <- function() {
  if (!requireNamespace("icd.data")) {
    message("icd.data doesn't appear to be available")
    return()
  }
  if (!icd_data_ver_ok()) {
    "2016"
  } else {
    get("get_icd10cm_active_ver", envir = asNamespace("icd.data"))()
  }
}

.idget <- function(var_name, alt = NULL, must_work = is.null(alt), ...) {
  # this should only happen in weird R CMD check internals since we Import
  # icd.data...
  if (!requireNamespace("icd.data")) {
    message("icd.data doesn't appear to be available")
    return(.idget(alt))
  }
  if (!icd_data_ver_ok()) {
    message("This data is not available with icd.data version < 1.1")
    message("Upgrade icd.data using install.packages(\"icd.data\"")
    if (must_work) stop("Cannot proceed")
    return(.idget(alt))
  }
  # bindings are in the top level namespace
  # lazy data will only be at top level if package is attached
  ns <- asNamespace("icd.data")
  if (exists(var_name, ns, ...)) return(get(var_name, ns, ...))
  lz <- ns$.__NAMESPACE__.$lazydata
  # if icd.data >= 1.1, then we now assume it must be in lazy data
  get(var_name, lz, ...)
}

with_icd10cm_version <- function(ver, code) {
  stopifnot(is.character(ver), length(ver) == 1)
  old <- options("icd.data.icd10cm_active_ver" = ver)
  on.exit(options(old), add = TRUE)
  force(code)
}
