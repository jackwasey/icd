#' benchmark and profile major functions with larger data sets
#'
#' \code{icd9} package is intended to be used with large data sets, with
#' millions or rows. Performance of core functions is therefore of some
#' importance, after correctness. R package test code is for correctness,
#' whereas this script stresses the core functions, and looks for bottlenecks.

icd9Benchmark <- function() {
  library(microbenchmark)
  # generate large data set: this is copied from test-ICD9.R for now...
  set.seed(1441)
  n <- 1E7 # 10 million rows

  rpts <- randomPatients(n)

  tmp <- tempfile(fileext = ".Rprof")
  Rprof(filename = tmp, line.profiling = TRUE, memory.profiling = TRUE)
  capture.output(icd9Comorbidities(rpts))
  Rprof(NULL)

  #summaryRprof(filename = tmp, memory = "stats", lines = "both")
  summaryRprof(filename = tmp, memory = "both", lines = "show")

  #microbenchmark(times = 10, icd9ShortToParts(randomShortIcd9(5E+5)))
  #microbenchmark(times = 10, icd9ShortToPartsSlow(randomShortIcd9(5E+5)))
  #microbenchmark(times = 10, icd9ShortToPartsList(randomShortIcd9(5E+5)))

  microbenchmark::microbenchmark(times = 50, trim(randomShortIcd9))
  microbenchmark::microbenchmark(times = 50, strip(randomShortIcd9))

  # initializing empty data frame
  microbenchmark::microbenchmark(data.frame(matrix(ncol = 2, nrow = 100000)))
  microbenchmark::microbenchmark(data.frame(major = character(100000),
                            minor = character(100000)))

}

randomPatients <- function(n = 50000, np = 20) {
  pts <- round(n / np)
  data.frame(
    visitId = sample(seq(1, pts), replace = TRUE, size = n),
    icd9 = randomShortIcd9(n),
    poa = as.factor(
      sample(x = c("Y","N", "n", "n", "y", "X","E","",NA),
             replace = TRUE, size = n))
  )
}

randomShortIcd9 <- function(n = 50000)
  as.character(floor(runif(min = 1, max = 99999, n = n)))

randomDecimalIcd9 <- function(n = 50000)
  paste(
    round(runif(min = 1, max = 999, n = n)),
    sample(icd9ExpandMinor(), replace = TRUE, size = n),
    sep = "."
  )
