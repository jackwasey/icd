# what is my popularity?
popularity <- function(start = "2015-01-01", end = "2015-02-07", log_folder = "~/.rstudio.cran.log") {
  installr::download_RStudio_CRAN_data(start, end, log_folder = log_folder, override = FALSE)
  log_dat <- installr::read_RStudio_CRAN_data(log_folder, packages = "icd9")
  clean_log <- installr::format_RStudio_CRAN_data(log_dat)
}
