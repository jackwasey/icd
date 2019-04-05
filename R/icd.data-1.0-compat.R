icd_data_icd9cm_leaf_v32 <- function() {
  if (exists("icd9cm2014_leaf")) return(get("icd9cm2014_leaf"))
  get_icd9cm2014_leaf()
}

# alt is alternative data, not a variable name.
.idget <- function(var_name, alt = NULL, must_work = is.null(alt), ...) {
  # this should only happen in weird R CMD check internals since we Import
  # icd.data...
  if (!.idexists_either(var_name)) {
    msg <- paste(
      "This data is not available with icd.data version < 1.1",
      "Upgrade icd.data using install.packages(\"icd.data\"."
    )
    if (must_work) stop(msg, " Cannot proceed.")
    return(alt)
  }
  .idget_either(var_name)
}

.idget_lazy <- function(var_name) {
  ns <- asNamespace("icd")
  lz <- ns$.__NAMESPACE__.$lazydata
  get(var_name, lz)
}

.idexists_lazy <- function(var_name) {
  ns <- asNamespace("icd")
  lz <- ns$.__NAMESPACE__.$lazydata
  exists(var_name, lz)
}

.idget_either <- function(var_name) {
  ns <- asNamespace("icd")
  if (.idexists_lazy(var_name)) return(.idget_lazy(var_name))
  if (exists(var_name, ns)) return(get(var_name, ns))
  var_name <- paste0("get_", var_name)
  if (exists(var_name, ns)) return(get(var_name, ns, mode = "function")())
  stop(var_name, " not available in icd.data regular or lazy data")
}

.idexists_either <- function(var_name) {
  ns <- asNamespace("icd")
  .idexists_lazy(var_name) ||
    exists(var_name, ns) ||
    exists(paste0("get_", var_name), ns, mode = "function")
}
