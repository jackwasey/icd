# generate a bunch of random patients and get comorbidities, intended for valgrind usage
library(icd)
library(microbenchmark)
pts <- icd:::generate_random_pts(5e5)

# future benchmark for caching C++ data structures using 'cereal' memoisation
microbenchmark(icd_comorbid_ahrq(pts, cereal = FALSE),
	       icd_comorbid_ahrq(pts, cereal = TRUE),
	       times = 5L)
