library(profr)
library(microbenchmark)

# the slow code we are trying to speed up is:
# icd9cm_generate_chapters_hierarchy()
# of which icd9_get_chapters is the slowest step?


Rprof(filename = "/tmp/gh.txt", interval = 0.001, line.profiling = TRUE)
j <- icd:::icd9cm_generate_chapters_hierarchy()
Rprof(NULL)
summaryRprof("/tmp/gh.txt", lines = "show")

test_codes <- icd9cm_hierarchy[icd9cm_hierarchy$billable, "code"]

prf <- profr(
  icd9_get_chapters(x = test_codes[1:1000], short_code = TRUE),
  interval = 0.001
)
print(prf)

# same with Rprof
Rprof(filename = "/tmp/gc.txt", interval = 0.001, line.profiling = TRUE)
icd9_get_chapters(x = test_codes, short_code = TRUE)
Rprof(NULL)
summaryRprof("/tmp/gc.txt", lines = "show")


# actually %i9mj% is the slowest part

microbenchmark("100" %i9mj% "100",
  "100" %i9mj% "110",
  "100" %i9mj% "999",
  icd:::expand_range_major.icd9("100", "999", defined = TRUE),
  icd:::expand_range_major.icd9("100", "999", defined = FALSE),
  times = 50
)

# baseline results:

# Unit: milliseconds
# expr       min        lq     mean    median       uq      max neval
# "100" %i9mj% "100"  8.699477  9.635996 12.76972 10.092612 11.82888 50.40620    50
# "100" %i9mj% "110"  8.851840  9.390647 11.51193  9.884331 10.92320 28.38282    50
# "100" %i9mj% "999" 17.907386 19.016562 25.47287 21.088939 26.57372 78.33338    50

# prf_i9mj <- profr("100" %i9mj% "110"))
# prf_mj <- profr({
# icd:::icd_expand_range_major.icd9("100", "110", defined = TRUE)
#  }) #, interval = 0.01, quiet = TRUE)

Rprof(filename = "/tmp/mj.txt", interval = 0.001, line.profiling = TRUE)
microbenchmark(
  icd:::expand_range_major.icd9("100", "999", defined = TRUE),
  times = 100
)
Rprof(NULL)
summaryRprof("/tmp/mj.txt", lines = "show")
