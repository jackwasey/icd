# The comorbidity package optionally uses a parallel flag. Here we benchmark
# comorbidity against itself with and without parallel so we can choose the best
# option to compare to 'icd'. Unfortunately, both are slow, so this may take
# many minutes or hours, depending on hardware.

# The 'install-dependencies.R' script should be run before this.

n <- 10^(0L:5L)
cmb_res <- bench::press(n = n, {
  pts <- ten_million_random_pts[seq_len(n), ]
  bench::mark(
    comorbidity::comorbidity(
      x = pts, id = "visit_id", code = "code", score = "charlson_icd9",
      parallel = TRUE),
    comorbidity::comorbidity(
      x = pts, id = "visit_id", code = "code", score = "charlson_icd9",
      parallel = FALSE)
  )})
