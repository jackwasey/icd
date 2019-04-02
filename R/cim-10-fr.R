.dl_icd10fr2019 <- function(save_data = TRUE, ...) {
  if (!.confirm_download()) return()
  .unzip_to_data_raw(
    url = paste(
      sep = "/",
      "https://www.atih.sante.fr",
      "plateformes-de-transmission-et-logiciels",
      "logiciels-espace-de-telechargement",
      "telecharger/gratuit/11616/456"
    ),
    file_name = "LIBCIM10MULTI.TXT",
    dl_msg = "Downloading ICD-10-FR (CIM-10-FR)",
    ...
  )
}

#' Read the definitions of the French edition of ICD-10
#'
#' The short descriptions are capitalized, with accented characters, so leaving
#' as is.
#' @keywords internal
#' @noRd
.parse_icd10fr2019 <- function(must_work = FALSE,
                               ...) {
  fp <- .dl_icd10fr2019(...)
  if (is.null(fp)) {
    if (must_work) {
      stop("Unable to get data to parse ICD-10-FR (CIM-10-FR) 2019")
    }
    return(NULL)
  }
  cim_raw <- utils::read.delim(
    fileEncoding = "Latin1",
    fp$file_path,
    header = FALSE,
    sep = "|",
    as.is = TRUE
  )
  icd10fr2019 <- cim_raw[c(1, 5, 6)]
  names(icd10fr2019) <- c("code", "short_desc", "long_desc")
  icd10fr2019$short_desc <- enc2utf8(icd10fr2019$short_desc)
  icd10fr2019$long_desc <- enc2utf8(icd10fr2019$long_desc)
  icd10fr2019[["code"]] <- trimws(icd10fr2019[["code"]])
  icd10fr2019$major <- ""
  icd10fr2019$three_digit <- ""
  current_major <- NULL
  current_three_digit <- NULL
  for (i in seq_along(icd10fr2019$code)) {
    icd <- icd10fr2019[i, "code"]
    if (nchar(icd) == 3L) {
      current_major <- icd10fr2019[i, "long_desc"]
      current_three_digit <- icd
    }
    icd10fr2019[i, "major"] <- current_major
    icd10fr2019[i, "three_digit"] <- current_three_digit
  }
  icd10fr2019$major <- factor(icd10fr2019$major)
  # TODO: chapitres
  class(icd10fr2019$code) <- c("icd10fr", "icd10", "character")
  class(icd10fr2019$three_digit) <- c("icd10fr", "icd10", "character")
  .save_in_resource_dir(icd10fr2019)
  invisible(icd10fr2019)
}

#' Use the WHO ICD-10 French 'hierarchy' flat file to infer chapter ranges
#'
#' These are UTF-8 encoded. If there are no UTF-8 characters, it seems that R
#' forces the 'unknown' encoding label. This could be scraped from the web site
#' directly, which is what \code{.fetch_who_api_chapters()} does, but this is
#' at least as good.
#' @keywords internal
#' @noRd
.get_chapters_fr <- function(save_data = FALSE) {
  icd10_chapters_fr <- .get_chapter_ranges_from_flat(
    flat_hier = get_icd10who2008fr(),
    field = "chapter"
  )
  .save_in_data_dir(icd10_chapters_fr)
  invisible(icd10_chapters_fr)
}

.get_sub_chapters_fr <- function(save_data = FALSE) {
  icd10_sub_chapters_fr <- .get_chapter_ranges_from_flat(
    flat_hier = get_icd10who2008fr(),
    field = "sub_chapter"
  )
  .save_in_data_dir(icd10_sub_chapters_fr)
  invisible(icd10_sub_chapters_fr)
}
