
print.comorbidity_map <- function(comorbidity_map, summary = TRUE, n_comorbidities = 7, n_codes = 7) {
  if (summary) {
    message("Showing first ", n_comorbidities, "comorbidities, and ", n_codes, "of each one.")
    comorbidity_map %>%
      lapply(function(x) get_n_or_len(x, n_codes)) %>%
      get_n_or_len(n_comorbidities) %>%
      as.list %>% print
    # not beautiful
    if (length(comorbidity_map) > n_comorbidities)
      cat("...\n")
  }
  else
    print(as.list(comorbidity_map))
}

get_n_or_len <- function(x, n)
  x[1:ifelse(length(x) < n, length(x), n)]
