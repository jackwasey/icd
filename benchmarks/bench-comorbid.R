# generate a bunch of random patients and get comorbidities, intended for valgrind usage
library(icd)
library(microbenchmark)
pts <- icd:::generate_random_pts(1e7)
message("benchmark starting!")
microbenchmark(icd_comorbid(pts, icd9_map_ahrq, comorbid_fun = icd:::icd9ComorbidTaskloop),
               icd_comorbid(pts, icd9_map_ahrq, comorbid_fun = icd:::icd9ComorbidShortCpp),
               times = 5L)
