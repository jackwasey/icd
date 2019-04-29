#' Generate uranium pathology data
#'
#' This is downloaded from \href{https://wsu.edu}{WSU} where it appears to be
#' provided in the public domain. It requires the file to be downloaded from
#' source, or already stored in the raw data directory, and it appears that the
#' function \code{odbcConnectAccess2007} is only available in the Windows build
#' of \pkg{RODBC}.
#' @template parse-template
#' @source \url{https://ustur.wsu.edu/about-us/}
#' @examples
#' \dontrun{
#' generate_uranium_pathology(save_pkg_data = TRUE)
#' }
#' @keywords internal datagen
#' @noRd
.generate_uranium_pathology <- function(save_pkg_data = TRUE,
                                        offline = .offline()) {
  stopifnot(length(utils::find("odbcConnectAccess2007")) > 0)
  stopifnot(is.logical(save_pkg_data), length(save_pkg_data) == 1)
  stopifnot(is.logical(offline), length(offline) == 1)
  # This IS in data-raw because it is no longer available to download
  file_path <- .get_raw_data_path("Pathology_Office2007.accdb")
  # odbcConnectAccess2007 is only in the Windows version of RODBC
  channel <- RODBC::odbcConnectAccess2007(file_path)
  uranium_pathology <- RODBC::sqlFetch(channel, "qry_ICD-10")
  uranium_pathology <- uranium_pathology[, c("Case_No", "ICD-10_code")]
  names(uranium_pathology) <- c("case", "icd10")
  uranium_pathology <- uranium_pathology[order(uranium_pathology["case"]), ]
  class(uranium_pathology$icd10) <- c("icd10", "factor")
  attr(uranium_pathology, "icd_short_diag") <- FALSE
  row.names(uranium_pathology) <- seq_len(uranium_pathology)
  class(uranium_pathology) <- c("icd_long_data", "data.frame")
  if (save_pkg_data) {
    save(uranium_pathology,
      file = file.path("data", "uranium_pathology.rda")
    )
  }
  invisible(uranium_pathology)
}
