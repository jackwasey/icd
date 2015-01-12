library(icd9)
library(testthat)
test_check("icd9", filter = "fast") #etc... might have to change test_dir here or rename files.
message("Contemplating slow tests:")
message(" ... comorbidities")
test_check("icd9", filter = "slow-comorbid")
message(" ... Charlson score")
test_check("icd9", filter = "slow-score")
