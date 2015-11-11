
print.comorbidity_map <- function(comorbidity_map, summary = TRUE, n_comorbidities = 7, n_codes = 7) {
  if (summary) {
    comorbidity_map_summlapply(comorbidity_map, function(x) c(comorbidity_map)

    )
    print(as.list(get_n_or_len(comorbidity_map, n_comorbidities)))
  }
  else
    print(as.list(comorbidity_map))
}

get_n_or_len <- function(x, n)
  x[1:ifelse(length(x) < n, length(x), n)]
