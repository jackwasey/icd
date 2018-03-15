# generate a bunch of random patients and get comorbidities, intended for valgrind usage
library(icd)
library(microbenchmark)
pts <- icd:::generate_random_pts(5e5)

microbenchmark(icd_comorbid_ahrq(pts, cereal = FALSE),
	       icd_comorbid_ahrq(pts, cereal = TRUE),
	       times = 5L)

microbenchmark(icd9_comorbid_css(pts),
               icd9_comorbid_css(pts, type = "Multi", lvl = 1),
               icd9_comorbid_css(pts, type = "Multi", lvl = 2),
               icd9_comorbid_css(pts, type = "Multi", lvl = 3),
               icd9_comorbid_css(pts, type = "Multi", lvl = 4),
               times = 5L)
