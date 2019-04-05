#' Get all ICD-10-CM codes
#'
#' Gets all ICD-10-CM codes from an archive on the CDC web site.
#'
#' The factor generation uses \code{sort.default} which is locale dependent.
#' This meant a lot of time debugging a problem when white space was ignored for
#' sorting on some platforms, but not others.
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD10} also available from
#'   \url{http://www.cdc.gov/nchs/data/icd/icd10cm/2016/ICD10CM_FY2016_code_descriptions.zip}.
#' @references
#'   \href{https://www.cms.gov/Medicare/Coding/ICD10/downloads/icd-10quickrefer.pdf}{CMS ICD-10 Quick Reference}
#'   \href{https://www.cdc.gov/nchs/icd/icd10cm.htm#FY\%202019\%20release\%20of\%20ICD-10-CM}{CDC copy of ICD-10-CM for 2019}
#' @keywords internal datagen
#' @noRd
.parse_icd10cm_all <- function(save_data = FALSE,
                               offline = .offline(),
                               verbose = .verbose(),
                               twentysixteen = FALSE,
                               ...) {
  if (verbose) message("Parsing all ICD-10-CM diagnostic codes.")
  yrs <- names(.icd10cm_sources)
  yrs <- if (twentysixteen) {
    "2016"
  } else {
    yrs[yrs %nin% "2016"]
  }
  out <- lapply(
    yrs,
    .parse_icd10cm_year,
    save_data = save_data,
    ...
  )
  names(out) <- yrs
  invisible(out)
}

.parse_icd10cm_year <- function(year, save_pkg_data = FALSE, ...) {
  stopifnot(is.numeric(year) || is.character(year))
  year <- as.character(year)
  stopifnot(as.character(year) %in% names(.icd10cm_sources))
  f_info <- .dl_icd10cm_year(year = year, dx = TRUE)
  if (is.null(f_info)) {
    .absent_action_switch(
      paste("No path to downloaded data. Options are: "),
      paste(names(.show_options()), unname(.show_options()),
        collapse = ",",
        sep = ":"
      )
    )
    return()
  }
  message("Working on parsing ICD-10-CM year: ", year)
  # readLines may muck up encoding, resulting in weird factor order generation
  # later?
  x <- readLines(con = f_info$file_path, encoding = "ASCII")
  if (.verbose()) message("Got flat file for year: ", year)
  stopifnot(all(Encoding(x) == "unknown"))
  dat <- data.frame(
    # id = substr(x, 1, 5),
    code = trimws(substr(x, 7, 13)),
    billable = trimws(substr(x, 14, 15)) == "1",
    short_desc = trimws(substr(x, 16, 76)),
    long_desc = trimws(substr(x, 77, stop = 1e5)),
    stringsAsFactors = FALSE
  )
  dat[["code"]] <-
    icd::as.short_diag(
      icd::as.icd10cm(dat[["code"]])
    )
  dat[["three_digit"]] <- factor(get_major.icd10(dat[["code"]]))
  # here we must re-factor so we don't have un-used levels in major
  dat[["major"]] <- factor(
    merge(
      x = dat["three_digit"],
      y = dat[c("code", "short_desc")],
      by.x = "three_digit", by.y = "code",
      all.x = TRUE
    )[["short_desc"]]
  )
  dat[["major"]] <- icd::as.short_diag(icd::as.icd10cm(dat[["major"]]))
  if (.verbose()) message("Generating sub-chapter lookup for year: ", year)
  sc_lookup <- .icd10_generate_subchap_lookup()
  mismatch_sub_chap <-
    dat$three_digit[which(dat$three_digit %nin% sc_lookup$sc_major)]
  if (length(mismatch_sub_chap) != 0L) browser()
  dat[["sub_chapter"]] <-
    merge(
      x = dat["three_digit"],
      y = sc_lookup,
      by.x = "three_digit",
      by.y = "sc_major",
      all.x = TRUE
    )[["sc_desc"]]
  if (.verbose()) message("Generating chap lookup for year: ", year)
  chap_lookup <- .icd10_generate_chap_lookup()
  dat[["chapter"]] <-
    merge(dat["three_digit"], chap_lookup,
      by.x = "three_digit", by.y = "chap_major",
      all.x = TRUE
    )[["chap_desc"]]
  dat <- dat[order.icd10cm(dat$code), ]
  class(dat$code) <- c("icd10cm", "icd10", "character")
  row.names(dat) <- NULL
  .save_in_resource_dir(var_name = paste0("icd10cm", year), x = dat)
  if (save_pkg_data && year == "2016") .save_in_data_dir("icd10cm2016", compress = "xz")
  invisible(dat)
}
