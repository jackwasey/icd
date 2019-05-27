#' Generate \code{vermont_dx} data
#'
#' Process data from \href{healthvermont.gov}{Health Vermont}
#' @template parse-template
#' @examples
#' \dontrun{
#' generate_vermont_dx(save_pkg_data = TRUE)
#' }
#' @keywords internal datagen
#' @noRd
.generate_vermont_dx <- function(save_pkg_data) {
  # This is indeed in data-raw, because not available to download anymore
  vermont_fp <- .get_raw_data_path("VTINP13.TXT")
  vermont_dx <- utils::read.csv(vermont_fp,
    stringsAsFactors = FALSE,
    strip.white = TRUE
  )
  vermont_dx <- vermont_dx[1:1000, c(74, 4, 6, 7, 11, 13:32)]
  age_group <- vermont_dx$intage
  attr(age_group, "class") <- "factor"
  attr(age_group, "levels") <- c(
    "Under 1", "1-17", "18-24",
    "25-29", "30-34", "35-39",
    "40-44", "45-49", "50-54",
    "55-59", "60-64", "65-69",
    "70-74", "75 and over",
    "Unknown"
  )
  age_group <- ordered(age_group)
  sex <- vermont_dx$sex
  attr(sex, "class") <- "factor"
  attr(sex, "levels") <- c("male", "female", "unknown")
  vermont_dx$intage <- age_group
  vermont_dx$sex <- sex
  # death = 8 (other codes are for various discharge statuses)
  vermont_dx$dstat <- vermont_dx$dstat == 8
  names(vermont_dx)[seq_len(5)] <- c(
    "visit_id",
    "age_group",
    "sex",
    "death",
    "DRG"
  )
  class(vermont_dx) <- c("icd_wide_data", "data.frame")
  dx_cols <- paste0("DX", seq_len(20))
  for (dc in dx_cols) {
    class(vermont_dx[[dc]]) <- c("icd9cm", "icd9", "character")
    attr(vermont_dx[[dc]], "icd_short_diag") <- TRUE
  }
  if (save_pkg_data) .save_in_data_dir(vermont_dx)
  invisible(vermont_dx)
}
