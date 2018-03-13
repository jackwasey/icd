# generate a bunch of random patients and get comorbidities, intended for valgrind usage
library(icd)
library(microbenchmark)
pts <- icd:::generate_random_pts(5e7)

microbenchmark(icd_comorbid(vt, icd9_map_ahrq),
               icd_comorbid(vt, icd9_map_ahrq, comorbid_fun = icd:::icd9ComorbidShortCpp),
               times = 5L)

res1 <- icd_comorbid(vt, icd9_map_ahrq, comorbid_fun = icd:::icd9ComorbidTaskloop)
res2 <- icd_comorbid(vt, icd9_map_ahrq, comorbid_fun = icd:::icd9ComorbidShortCpp)
identical(res1, res2)
