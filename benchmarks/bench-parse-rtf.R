library(profr)

prf_rtf <- profr::profr(rtf_parse_year(year = "2011", save_data = FALSE, verbose = FALSE))
print(head(prf_rtf))

Rprof(filename = "/tmp/rtf.txt", interval = 0.001, line.profiling = TRUE)
rtf_parse_year(year = "2011", save_data = FALSE, verbose = FALSE)
Rprof(NULL)
summaryRprof("/tmp/rtf.txt", lines = "show")
