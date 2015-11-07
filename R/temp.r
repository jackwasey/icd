
temp <- function() {
  library(magrittr)

  # http://www.cdc.gov/nchs/data/icd/icd10cm/2016/ICD10CM_FY2016_code_descriptions.zip

  #zip_single("http://www.cdc.gov/nchs/data/icd/icd10cm/2016/ICD10CM_FY2016_code_descriptions.zip", "icd10cm_order_2016.txt", "/home/jack/Documents/RProjects/icd9/inst/extdata/icd10.txt")
  #  awk '{print $2}' inst/extdata/icd10.txt > icd10codes.txt

  icd10 <- read.csv("inst/extdata/icd10codes.txt")
  i10 <- as.character(icd10$A00)

  #alpha_in_tail <- grep("[[:alpha:]]", i10tail, value = TRUE)
  alpha_in_tail_bool <- grepl("[[:alpha:]].*[[:alpha:]].*", x = i10)
  alpha_in_tail <- i10[alpha_in_tail_bool]
  unique(gsub("[[:digit:]]", replacement = "", x = alpha_in_tail))

  # verify, e.g. J in middle?
  grep("[[:alpha::]].*J.*", i10)

  # find unique characters at each position from 4 to 7

  for (i in 1:7) {
    message(i)
    substring(alpha_in_tail, i, i) %>% unique %>% sort %>% message
  }

}
