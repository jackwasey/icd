# compare environement to standard %in% search for RTF parsing

requireNamespace("microbenchmark")
load("benchmarks/rtf-fourth-out.rda")
load("benchmarks/lookup-fourth.rda")
print(microbenchmark::microbenchmark(rtf_lookup_fourth(out = rtf_fourth_out, lookup_fourth = lookup_fourth, verbose = FALSE),
  rtf_lookup_fourth_alt_env(out = rtf_fourth_out, lookup_fourth = lookup_fourth, verbose = FALSE),
  times = 100L
))
stopifnot(identical(
  rtf_lookup_fourth(out = rtf_fourth_out, lookup_fourth = lookup_fourth, verbose = FALSE),
  rtf_lookup_fourth_alt_env(out = rtf_fourth_out, lookup_fourth = lookup_fourth, verbose = FALSE)
))
