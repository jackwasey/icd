icd10_parse_ahrq_pcs <- function(save_data = FALSE) {
  f <- unzip_to_data_raw(
    url = paste0(
      "https://www.hcup-us.ahrq.gov/toolssoftware/",
      "procedureicd10/pc_icd10pcs_2018_1.zip"
    ),
    file_name = "pc_icd10pcs_2018.csv", offline = !save_data
  )
  dat <- read.csv(
    file = f$file_path, skip = 1, stringsAsFactors = FALSE,
    colClasses = "character", encoding = "latin1"
  )
  names(dat) <- c("code", "desc", "class_number", "class")
  dat$class <- factor(dat$class,
    levels = c(
      "Minor Diagnostic",
      "Minor Therapeutic",
      "Major Diagnostic",
      "Major Therapeutic"
    )
  )
  dat$class_number <- NULL
  dat$code <- gsub(dat$code, pattern = "'", replacement = "")
  icd10_map_ahrq_pcs <- split(dat$code, dat$class)
  if (save_data) {
    .save_in_data_dir(icd10_map_ahrq_pcs)
  }
}
