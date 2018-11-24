R --slave --vanilla -d lldb -e "library(icd); library(magrittr); icd_comorbid_ahrq(vermont_dx, comorbid_fun = icd:::comorbidMatMul)"

