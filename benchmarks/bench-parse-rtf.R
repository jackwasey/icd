library(profr)

prf_rtf <- profr::profr(rtf_parse_year(year = "2011"))
print(head(prf_rtf))

Rprof(filename = "/tmp/rtf.txt", interval = 0.001, line.profiling = TRUE)
rtf_parse_year(year = "2011")
Rprof(NULL)
summaryRprof("/tmp/rtf.txt", lines = "show")
