library(icd9)
library(magrittr)
library(testthat)
test_check("icd9", filter = "fast") #etc... might have to change test_dir here or rename files.
test_check("icd9", filter = "slow") #etc... might have to change test_dir here or rename files.
