Rscript -e "library(icd); library(magrittr); vermont_dx %>% icd_wide_to_long %>% icd_charlson"

