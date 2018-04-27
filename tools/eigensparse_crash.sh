R --slave --vanilla -d lldb -e "library(icd); library(magrittr); icd_comorbid_ahrq(vermont_dx %>% wide_to_long, comorbid_fun = icd:::comorbidMatMul)"

