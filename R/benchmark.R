#' benchmark and profile major functions with larger data sets
#'
#' \code{icd9} package is intended to be used with large data sets, with
#' millions or rows. Performance of core functions is therefore of some
#' importance, after correctness. R package test code is for correctness,
#' whereas this script stresses the core functions, and looks for bottlenecks.

icd9Benchmark <- function() {
  library(microbenchmark)
  library(profr)
  # generate large data set: this is copied from test-ICD9.R for now...
  set.seed(1441)
  n <- 1E7 # 10 million rows

  rpts <- randomPatients(n)


  # run slow tests
  res <- test_dir("tests/testthat/", filter = "slow", reporter = ListReporter())
  res <- as.data.frame(res)
  print(res[order(res$real), c("test", "real")])

  tmp <- tempfile(fileext = ".Rprof")
  Rprof(filename = tmp, line.profiling = TRUE, memory.profiling = TRUE)
  capture.output(icd9ComorbidAhrq(rpts, isShort = TRUE))
  Rprof(NULL)
  #summaryRprof(filename = tmp, memory = "stats", lines = "both")
  summaryRprof(filename = tmp, memory = "both", lines = "show")


  tmp <- tempfile(fileext = ".Rprof")
  Rprof(filename = tmp, line.profiling = TRUE, memory.profiling = FALSE)
  capture.output(icd9ChildrenShort("300" %i9s% "450"))
  Rprof(NULL)
  summaryRprof(filename = tmp, lines = "show")

  mydf <- data.frame(visitId = c("a", "b", "c"),
                     icd9 = c("441", "412.93", "044.9"),
                     stringsAsFactors = TRUE)
  prfCharl <- profr::profr(icd9Charlson(mydf,
                                 return.df = TRUE,
                                 stringsAsFactors = TRUE,
                                 isShort = FALSE))
  ggplot2::ggplot(prfCharl, minlabel = 0.04)

  rng <- "300" %i9s% "450"
  prfChild <- profr::profr(icd9ChildrenShort(rng))
  ggplot(prfChild, minlabel = 0.001)
  ggsave("tmpggplot.jpg", width = 250, height=5, dpi=200, limitsize = FALSE)

  microbenchmark::microbenchmark(times = 20,
                 icd9PartsRecompose(data.frame(major = rep(as.character(100:999), times = 250),
                                               minor = rep("01", times = 900 * 250)),
                                    isShort = T)
  )
  microbenchmark::microbenchmark(times = 1,
                                 icd9PartsRecompose(data.frame(major = as.character(100:999),
                                                               minor = rep(NA, times = 900)),
                                                    isShort = T)
  )

  microbenchmark::microbenchmark(times = 500, # initial about 2ms
                                 icd9AddLeadingZeroesMajor(major = c(1 %i9mj% 999, paste("V", 1:9, sep=""))))

  microbenchmark::microbenchmark(times = 500, # initial about 2ms
                                 icd9AddLeadingZeroesMajor(major = c(1 %i9mj% 999, paste("V", 1:9, sep="")), addZeroV = TRUE))

  # 3.5 sec in v0.5, 2.7 sec without validation checks
  microbenchmark::microbenchmark(times = 5, icd9ChildrenShort("400" %i9s% "450"))


  #sprintf wins
  microbenchmark::microbenchmark(times = 500000, sprintf("%s%s", "410", "01"))
  microbenchmark::microbenchmark(times = 500000, paste("410", "01", sep = ""))

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
