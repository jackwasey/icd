# generate a bunch of random patients and get comorbidities, intended for valgrind usage
library(icd)
library(microbenchmark)

pts <- icd:::generate_random_pts(5e5)
message("benchmark starting!")
mb <- microbenchmark(
  icd_comorbid(pts, icd9_map_ahrq, comorbid_fun = icd:::icd9ComorbidTaskloop),
  icd_comorbid(pts, icd9_map_ahrq, comorbid_fun = icd:::icd9ComorbidTaskloop2),
  icd_comorbid(pts, icd9_map_ahrq, comorbid_fun = icd:::icd9ComorbidShortCpp),
  times = 5L)
print(mb)
