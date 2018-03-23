R --slave --vanilla -d "gdb -ex run" -e "library(icd); library(magrittr); head(vermont_dx) %>% icd_wide_to_long %>% icd_charlson"
# R --slave -e "tools::buildVignette(file = '$ICD_HOME/vignettes/Charlson_and_other_scores.Rmd', clean = FALSE, dir = '$ICD_HOME')"
# now I've hacker /usr/bin/R to always run the gdb debugger with "-ex run" (automatically run code on start).
# R --slave -e "devtools::build()"

