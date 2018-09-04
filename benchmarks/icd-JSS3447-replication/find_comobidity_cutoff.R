# The comorbidity package optionally uses a parallel flag. Here we benchmark
# comorbidity against itself with and without parallel so we can choose the best
# option to compare to 'icd'. Unfortunately, both are slow, so this may take
# many minutes or hours, depending on hardware.

# The 'install-dependencies.R' and 'bench-versus.R" scripts should be run first.

n <- 10^(0L:5L)
cmb_res <- bench::press(n = n, {
  pts <- get_pts(n, dz_per_pt = 20)
  bench::mark(
    comorbidity::comorbidity(
      x = pts, id = "visit_id", code = "code", score = "charlson_icd9",
      parallel = TRUE),
    comorbidity::comorbidity(
      x = pts, id = "visit_id", code = "code", score = "charlson_icd9",
      parallel = FALSE)
  )
})
print(cmb_res)
