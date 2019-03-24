icd_data_icd10cm_active <- function() {
  if (!requireNamespace("icd.data")) {
    message("icd.data doesn't appear to be available")
    return()
  }
  if (utils::packageVersion("icd.data") < "1.1") {
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
  # if (utils::packageVersion("icd.data") < "1.1")
  #   icd.data::icd9cm_billable[["32"]]
  # else
  #   get("icd9cm_leaf_v32", envir = asNamespace("icd.data"))
}

icd_data_get_icd10cm_active_ver <- function() {
  if (!requireNamespace("icd.data")) {
    message("icd.data doesn't appear to be available")
    return()
  }
  if (utils::packageVersion("icd.data") < "1.1") {
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
  if (utils::packageVersion("icd.data") < "1.1") {
    message("This data is not available with icd.data version < 1.1")
    message("Upgrade icd.data using install.packages(\"icd.data\"")
    if (must_work) stop("Cannot proceed")
    return(.idget(alt))
  }
  # bindings are in the top level namespace
  # lazy data will only be at top level if package is attached
  ns <- asNamespace("icd.data")
  if (exists(var_name, ns)) return(get(var_name, ns, ...))
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
