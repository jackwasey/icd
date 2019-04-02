
.icd10cm_parse_cms_pcs_all <- function(save_data = FALSE,
                                       verbose = .verbose()) {
  if (verbose) message("Parsing all ICD-10-CM procedure codes")
  lapply(names(.icd10cm_sources),
    .icd10cm_parse_cms_pcs_year,
    save_data = save_data,
    verbose = verbose
  )
  invisible()
}

.icd10cm_parse_cms_pcs_year <- function(year,
                                        must_work = FALSE,
                                        verbose = .verbose()) {
  year <- as.character(year)
  message("Please wait a few moments to parse and cache data.")
  pcs_file <- .icd10cm_sources[[year]][["pcs_flat"]]
  pcs_path <- file.path(
    icd_data_dir(),
    .get_versioned_raw_file_name(pcs_file, ver = year)
  )
  out <- utils::read.fwf(pcs_path, c(5, 8, 2, 62, 120),
    header = FALSE,
    col.names = c(
      "count",
      "code",
      "leaf",
      "short_desc",
      "long_desc"
    )
  )
  out$count <- NULL
  out$code <- trimws(as.character(out$code))
  out$leaf <- as.logical(out$leaf)
  out$short_desc <- trimws(as.character(out$short_desc))
  out$long_desc <- trimws(as.character(out$long_desc))
  out <- out[order(out$code), ]
  var_name <- paste0("icd10cm", year, "_pc")
  .save_in_resource_dir(var_name = var_name, x = out)
  invisible(out)
}
