library(profr)

prf_rtf <- profr(parse_rtf_year(year = "2011", save_data = FALSE, verbose = FALSE))
print(head(prf_rtf))

Rprof(filename = "/tmp/rtf.txt", interval = 0.001, line.profiling = TRUE)
parse_rtf_year(year = "2011", save_data = FALSE, verbose = FALSE)
Rprof(NULL)
summaryRprof("/tmp/rtf.txt", lines = "show")
