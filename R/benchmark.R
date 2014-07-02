#' benchmark and profile major functions with larger data sets
#'
#' \code{icd9} package is intended to be used with large data sets, with
#' millions or rows. Performance of core functions is therefore of some
#' importance, after correctness. R package test code is for correctness,
#' whereas this script stresses the core functions, and looks for bottlenecks.

icd9Benchmark <- function() {
  require(microbenchmark)
  # generate large data set: this is copied from test-ICD9.R for now...
  set.seed(1441)
  n <- 1E7 # 10 million rows
  np <- 20 # 20 icd9 codes per patient

  rpts <- randomPatients(n)

  tmp <- tempfile(fileext = ".Rprof")
  Rprof(filename = tmp, line.profiling = T, memory.profiling = T)
  capture.output(icd9Comorbidities(rpts))
  Rprof(NULL)

  #summaryRprof(filename = tmp, memory = "stats", lines = "both")
  summaryRprof(filename = tmp, memory = "both", lines = "show")

  #microbenchmark(times = 10, icd9ExtractPartsShort(randomShortIcd9(5E+5)))
  #microbenchmark(times = 10, icd9ExtractPartsShortSlow(randomShortIcd9(5E+5)))
  #microbenchmark(times = 10, icd9ExtractPartsShortList(randomShortIcd9(5E+5)))

  microbenchmark(times = 50, trim(randomShortIcd9))
  microbenchmark(times = 50, strip(randomShortIcd9))

  # initializing empty data frame
  microbenchmark(data.frame(matrix(ncol = 2, nrow = 100000)))
  microbenchmark(data.frame(major = character(100000), minor = character(100000)))

}

randomPatients <- function(n = 50000, np = 20) {
  pts <- round(n/np)
  data.frame(
    visitId = sample(seq(1, pts), replace = TRUE, size = n),
    icd9 = randomShortIcd9(n),
    poa = as.factor(
      sample(x = c("Y","N", "n", "n", "y", "X","E","",NA), replace = T, size = n))
  )
}

randomShortIcd9 <- function(n = 50000)
  as.character(floor(runif(min=1, max=99999, n=n))) # tolerate <3 digits?

randomDecimalIcd9 <- function(n = 50000)
  paste(
    round(runif(min = 1, max = 999, n = n)),
    sample(icd9ExpandMinor(), replace = T, size = n),
    sep = "."
  )
