#' \dontrun{
#' library(microbenchmark)
#' x <- icd:::generate_random_decimal_icd9(1e6)
#' microbenchmark(
#'   x %>% icd:::as_char_no_warn %>%
#'   stringr::str_replace("[[:space:]]*([EeVv]?)(0*)([\\.[:digit:]]*)[[:space:]]*", "\\1\\3"),
#'
#'   stringr::str_replace(icd:::as_char_no_warn(x),
#'               "[[:space:]]*([EeVv]?)(0*)([\\.[:digit:]]*)[[:space:]]*", "\\1\\3"),
#'
#'   gsub("[[:space:]]*([EeVv]?)(0*)([\\.[:digit:]]*)[[:space:]]*", "\\1\\3", x),
#'   times = 1000
#'   )
#' }

