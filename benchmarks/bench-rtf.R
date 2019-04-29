f_info_rtf <- rtf_fetch_year("2011", offline = FALSE)
rtf_lines <- readLines(f_info_rtf$file_path, warn = FALSE, encoding = "ASCII")
microbenchmark::microbenchmark(
  res_both <- rtf_parse_lines(rtf_lines, perl = TRUE, useBytes = TRUE),
  res_none <- rtf_parse_lines(rtf_lines, perl = FALSE, useBytes = FALSE),
  res_bytes <- rtf_parse_lines(rtf_lines, perl = FALSE, useBytes = TRUE),
  res_perl <- rtf_parse_lines(rtf_lines, perl = TRUE, useBytes = FALSE),
  times = 5
)
stopifnot(identical(res_both, res_none))

# rtf_fix_unicode is a slow step, useBytes and perl together is faster
f_info_rtf <- rtf_fetch_year("2011", offline = FALSE)
rtf_lines <- readLines(f_info_rtf$file_path, warn = FALSE, encoding = "ASCII")
microbenchmark::microbenchmark(
  res_both <- rtf_fix_unicode(rtf_lines, perl = TRUE, useBytes = TRUE),
  res_none <- rtf_fix_unicode(rtf_lines, perl = FALSE, useBytes = FALSE),
  res_bytes <- rtf_fix_unicode(rtf_lines, perl = FALSE, useBytes = TRUE),
  res_perl <- rtf_fix_unicode(rtf_lines, perl = TRUE, useBytes = FALSE),
  times = 5
)
stopifnot(identical(res_both, res_none))

#' # rtf_strip is a slow step, useBytes and perl together is five times faster
f_info_rtf <- rtf_fetch_year("2011", offline = FALSE)
rtf_lines <- readLines(f_info_rtf$file_path, warn = FALSE, encoding = "ASCII")
microbenchmark::microbenchmark(
  res_both <- rtf_strip(rtf_lines, perl = TRUE, useBytes = TRUE),
  res_none <- rtf_strip(rtf_lines, perl = FALSE, useBytes = FALSE),
  res_bytes <- rtf_strip(rtf_lines, perl = FALSE, useBytes = TRUE),
  res_perl <- rtf_strip(rtf_lines, perl = TRUE, useBytes = FALSE),
  times = 5
)
stopifnot(identical(res_both, res_none))

library(microbenchmark)
requireNamepsace("stringr")
# stringr::str_split is faster this time
x <- icd:::generate_random_decimal_icd9(10)
microbenchmark(strsplit(x, "\\."), stringr::str_split(x, "\\."))
microbenchmark(icd:::trim(x), stringr::str_trim(x))
