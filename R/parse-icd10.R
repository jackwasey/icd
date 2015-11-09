
#' get all ICD-10-CM codes
#'
#' gets all ICD-10-CM codes from an archive on the CDC web site at \url{http://www.cdc.gov/nchs/data/icd/icd10cm/2016/ICD10CM_FY2016_code_descriptions.zip}. Initially, this just grabs 2016.
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_trim
#' @references https://www.cms.gov/Medicare/Coding/ICD10/downloads/icd-10quickrefer.pdf
#' @keywords internal
icd10cm_get_all_real <- function(save = TRUE) {

  pkg_name <- getPackageName()
  if (pkg_name == ".GlobalEnv") pkg_name <- "icd9"
  extdata_path <- system.file("extdata", package = pkg_name)
  local_path <- file.path(extdata_path, "icd10cm_order_2016.txt")
  if (!file.exists(local_path))
    zip_single(url = "http://www.cdc.gov/nchs/data/icd/icd10cm/2016/ICD10CM_FY2016_code_descriptions.zip",
               filename = "icd10cm_order_2016.txt", save_path = local_path)

  raw <- readLines(con = local_path)
  icd10cm2016 <- data.frame(id = substr(raw, 1, 5),
                            code = substr(raw, 7, 13),
                            leaf = substr(raw, 14, 15),
                            descShort = substr(raw, 16, 76),
                            descLong = substr(raw, 77, stop = 1e5),
                            stringsAsFactors = FALSE
  )

  icd10cm2016 <- as.data.frame(lapply(icd10cm2016, stringr::str_trim), stringsAsFactors = FALSE)
  if (save) save_in_data_dir(icd10cm2016)
  return(invisible(icd10cm2016))

  # now some test code to see what permutations there are of ICD-10 codes based
  # on the 2016 CM set.
  i10 <- icd10cm2016$code

  #alpha_in_tail <- grep("[[:alpha:]]", i10tail, value = TRUE)
  alpha_in_tail_bool <- grepl("[[:alpha:]].*[[:alpha:]].*", x = i10)
  alpha_in_tail <- i10[alpha_in_tail_bool]
  unique(gsub("[[:digit:]]", replacement = "", x = alpha_in_tail))

  # verify, e.g. J in middle?
  grep("[[:alpha::]].*J.*", i10)

  # find unique characters at each position from 4 to 7
  # for (i in 1:7)
  #   message(i)
  #   substring(alpha_in_tail, i, i) %>% unique %>% sort %>% message
  # }
}
