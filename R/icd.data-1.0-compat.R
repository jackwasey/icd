icd_data_icd10cm_active <- function() {
  if (!requireNamespace("icd.data")) {
    message("icd.data doesn't appear to be available")
    return()
  }
  if (utils::packageVersion("icd.data") < "1.1") {
    return(icd.data::icd10cm2016)
  }
  get("icd10cm_active", envir = asNamespace("icd.data"))
}

.idget <- function(var_name, must_work = TRUE) {
  # this should only happen in weird R CMD check internals since we Import
  # icd.data...
  if (!requireNamespace("icd.data")) {
    message("icd.data doesn't appear to be available")
    return()
  }
  if (utils::packageVersion("icd.data") < "1.1") {
    message("This data is not available with icd.data version < 1.1")
    message("Upgrade icd.data using install.packages(\"icd.data\"")
    if (must_work) stop("Cannot proceed")
    return()
  }
  # bindings are in the top level namespace
  # lazy data will only be at top level if package is attached
  ns <- asNamespace("icd.data")
  if (exists(var_name, ns)) return(get(var_name, ns))
  lz <- ns$.__NAMESPACE__.$lazydata
  # if icd.data >= 1.1, then we now assume it must be in lazy data
  get(var_name, lz)
}
