# generate a bunch of random patients and get comorbidities, intended for valgrind usage
library(icd)
library(microbenchmark)

pts <- icd:::generate_random_pts(5e3)
message("benchmark starting!")
mb <- microbenchmark(
  icd_comorbid(pts, icd9_map_ahrq, comorbid_fun = icd:::icd9Comorbid_alt_Sparse),
  icd_comorbid(pts, icd9_map_ahrq, comorbid_fun = icd:::icd9Comorbid_alt_SparseOmp),
  icd_comorbid(pts, icd9_map_ahrq, comorbid_fun = icd:::icd9Comorbid_alt_Taskloop),
  icd_comorbid(pts, icd9_map_ahrq, comorbid_fun = icd:::icd9Comorbid_alt_Taskloop2),
  icd_comorbid(pts, icd9_map_ahrq, comorbid_fun = icd:::icd9ComorbidShortCpp),
  times = 5L)
print(mb)

# for 5e5 with openmp, sparse is slowest, but not multithreaded.
