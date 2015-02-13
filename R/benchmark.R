# EXCLUDE COVERAGE START

randomPatients <- function(n = 50000, np = 20) {
  set.seed(1441)
  pts <- round(n / np)
  data.frame(
    visitId = sample(seq(1, pts), replace = TRUE, size = n),
    icd9 = c(randomShortIcd9(round(n/2)), randomShortAhrq(n-round(n/2))),
    poa = as.factor(
      sample(x = c("Y","N", "n", "n", "y", "X","E","",NA),
             replace = TRUE, size = n)),
    stringsAsFactors = FALSE
  )
}

randomShortIcd9 <- function(n = 50000)
  as.character(floor(runif(min = 1, max = 99999, n = n)))

randomShortAhrq <- function(n = 50000)
  sample(unname(unlist(ahrqComorbid)), size = n, replace = TRUE)

randomDecimalIcd9 <- function(n = 50000)
  paste(
    round(runif(min = 1, max = 999, n = n)),
    sample(icd9ExpandMinor(), replace = TRUE, size = n),
    sep = "."
  )

runSetInt <- function(n = 4, np = 2) {
  pts <- randomPatients(n, np)
  icd9ComorbidShortRPSetInt(pts, ahrqComorbid)
}

runVecInt <- function(n = 4, np = 2, grain_size = 50) {
  pts <- randomPatients(n, np = np)
  icd9ComorbidShortRPVecInt(pts, ahrqComorbid, grainSize = grain_size)
}

runVecIntBatch <- function(n = 4, np = 2, grain_size = 0) {
  pts <- randomPatients(n, np = np)
  icd9ComorbidShortRPVecIntBatch(pts, ahrqComorbid, grainSize = grain_size)
}

runOpenMPVecInt <- function(n = 4, np = 2) {
  pts <- randomPatients(n, np = np)
  icd9ComorbidShortOpenMPVecInt(pts, ahrqComorbid)
}
benchComorbid <- function(n=1E5, threads = 1)
  system.time(icd9ComorbidShortParalelOpenMP(randomPatients(n), icd9Mapping = ahrqComorbid, threads = threads))

benchGrain <- function() {
  ptsHuge <- randomPatients(1000000, np = 17)
  microbenchmark(
    icd9ComorbidShortRPVecInt(ptsHuge, ahrqComorbid, grainSize = 2000),
    icd9ComorbidShortRPVecInt(ptsHuge, ahrqComorbid, grainSize = 1000),
    icd9ComorbidShortRPVecInt(ptsHuge, ahrqComorbid, grainSize = 500),
    icd9ComorbidShortRPVecInt(ptsHuge, ahrqComorbid, grainSize = 250),
    icd9ComorbidShortRPVecInt(ptsHuge, ahrqComorbid, grainSize = 125),
    icd9ComorbidShortRPVecInt(ptsHuge, ahrqComorbid, grainSize = 50),
    icd9ComorbidShortRPVecInt(ptsHuge, ahrqComorbid, grainSize = 25),
    icd9ComorbidShortRPVecInt(ptsHuge, ahrqComorbid, grainSize = 10),
    times = 3
    )
}

benchComorbidRcppParallel <- function() {
  ptsSmallOne <- randomPatients(1000, np = 1)
  ptsSmall <- randomPatients(1000, np = 20)
  ptsBigOne <- randomPatients(100000, np = 1)
  ptsBig <- randomPatients(100000, np = 20)
  ptsHugeOne <- randomPatients(1000000, np = 1)
  ptsHuge <- randomPatients(1000000, np = 20)

  microbenchmark(
    icd9ComorbidShort(ptsSmallOne, ahrqComorbid),
    icd9ComorbidShortRP(ptsSmallOne, ahrqComorbid),
    icd9ComorbidShortRPVecInt(ptsSmallOne, ahrqComorbid),
    icd9ComorbidShortRPVecIntBatch(ptsSmallOne, ahrqComorbid),
    icd9ComorbidShortRPSetInt(ptsSmallOne, ahrqComorbid),
    icd9ComorbidShortRPHalfVector(ptsSmallOne, ahrqComorbid),
    times = 100) %>% print

  microbenchmark(
    icd9ComorbidShort(ptsSmall, ahrqComorbid),
    icd9ComorbidShortRP(ptsSmall, ahrqComorbid),
    icd9ComorbidShortRPVecInt(ptsSmall, ahrqComorbid),
    icd9ComorbidShortRPVecIntBatch(ptsSmall, ahrqComorbid),
    icd9ComorbidShortRPSetInt(ptsSmall, ahrqComorbid),
    icd9ComorbidShortRPHalfVector(ptsSmall, ahrqComorbid),
    times = 20) %>% print

  microbenchmark(
    icd9ComorbidShort(ptsBig, ahrqComorbid),
    icd9ComorbidShortRP(ptsBig, ahrqComorbid),
    icd9ComorbidShortRPVecInt(ptsBig, ahrqComorbid),
    icd9ComorbidShortRPVecIntBatch(ptsBig, ahrqComorbid),
    icd9ComorbidShortRPSetInt(ptsBig, ahrqComorbid),
    icd9ComorbidShortRPHalfVector(ptsBig, ahrqComorbid),
    times = 5) %>% print

  microbenchmark(
    icd9ComorbidShort(ptsBigOne, ahrqComorbid),
    icd9ComorbidShortRP(ptsBigOne, ahrqComorbid),
    icd9ComorbidShortRPVecIntBatch(ptsBigOne, ahrqComorbid),
    icd9ComorbidShortRPVecInt(ptsBigOne, ahrqComorbid),
    icd9ComorbidShortRPSetInt(ptsBigOne, ahrqComorbid),
    icd9ComorbidShortRPHalfVector(ptsBigOne, ahrqComorbid),
    times = 3) %>% print

#     icd9ComorbidShort(ptsHuge, ahrqComorbid),
#     icd9ComorbidShortRP(ptsHuge, ahrqComorbid),
#     icd9ComorbidShortRPInteger(ptsHuge, ahrqComorbid),
#     icd9ComorbidShortRPHalfVector(ptsHuge, ahrqComorbid),

    # icd9ComorbidShortBoost(ptsSmall, ahrqComorbid), # very slow
    # icd9ComorbidShortBoost(ptsBig, ahrqComorbid), # very slow
    # icd9ComorbidShortBoost(ptsBigOne, ahrqComorbid),

}

icd9BenchComorbidParallelOpenMP <- function() {
  pts10000 <- randomPatients(10000)
  pts100000 <- randomPatients(100000)
  ptsBig <- randomPatients(500000)
  microbenchmark(
    icd9ComorbidShortParalelOpenMP(pts10000, ahrqComorbid, threads = 0),
    icd9ComorbidShortParalelOpenMP(pts10000, ahrqComorbid, threads = 1),
    icd9ComorbidShortParalelOpenMP(pts10000, ahrqComorbid, threads = 2),
    icd9ComorbidShortParalelOpenMP(pts10000, ahrqComorbid, threads = 4),
    icd9ComorbidShortParalelOpenMP(pts10000, ahrqComorbid, threads = 6),
    icd9ComorbidShortParalelOpenMP(pts100000, ahrqComorbid, threads = 0),
    icd9ComorbidShortParalelOpenMP(pts100000, ahrqComorbid, threads = 1),
    icd9ComorbidShortParalelOpenMP(pts100000, ahrqComorbid, threads = 2),
    icd9ComorbidShortParalelOpenMP(pts100000, ahrqComorbid, threads = 4),
    icd9ComorbidShortParalelOpenMP(pts100000, ahrqComorbid, threads = 6),
    icd9ComorbidShortParalelOpenMP(ptsBig, ahrqComorbid, threads = 0),
    icd9ComorbidShortParalelOpenMP(ptsBig, ahrqComorbid, threads = 1),
    icd9ComorbidShortParalelOpenMP(ptsBig, ahrqComorbid, threads = 2),
    icd9ComorbidShortParalelOpenMP(ptsBig, ahrqComorbid, threads = 4),
    icd9ComorbidShortParalelOpenMP(ptsBig, ahrqComorbid, threads = 6),
    # 8 is very slow (maxes hyperthreading...)
    times = 5) %>% print
}

icd9BenchComorbidParallelTwo <- function() {
  pts10000 <- randomPatients(10000)
  pts100000 <- randomPatients(100000)
  ptsBig <- randomPatients(500000)
  microbenchmark(
    icd9ComorbidShortParallelTwo(pts10000, ahrqComorbid, threads = 0),
    icd9ComorbidShortParallelTwo(pts10000, ahrqComorbid, threads = 1),
    icd9ComorbidShortParallelTwo(pts10000, ahrqComorbid, threads = 2),
    icd9ComorbidShortParallelTwo(pts10000, ahrqComorbid, threads = 4),
    icd9ComorbidShortParallelTwo(pts10000, ahrqComorbid, threads = 6),
    icd9ComorbidShortParallelTwo(pts100000, ahrqComorbid, threads = 0),
    icd9ComorbidShortParallelTwo(pts100000, ahrqComorbid, threads = 1),
    icd9ComorbidShortParallelTwo(pts100000, ahrqComorbid, threads = 2),
    icd9ComorbidShortParallelTwo(pts100000, ahrqComorbid, threads = 4),
    icd9ComorbidShortParallelTwo(pts100000, ahrqComorbid, threads = 6),
    icd9ComorbidShortParallelTwo(ptsBig, ahrqComorbid, threads = 0),
    icd9ComorbidShortParallelTwo(ptsBig, ahrqComorbid, threads = 1),
    icd9ComorbidShortParallelTwo(ptsBig, ahrqComorbid, threads = 2),
    icd9ComorbidShortParallelTwo(ptsBig, ahrqComorbid, threads = 4),
    icd9ComorbidShortParallelTwo(ptsBig, ahrqComorbid, threads = 6),
    # 8 is very slow (maxes hyperthreading...)
    times = 5) %>% print
}

#' benchmark and profile major functions with larger data sets
#'
#' \code{icd9} package is intended to be used with large data sets, with
#' millions or rows. Performance of core functions is therefore of some
#' importance, after correctness. R package test code is for correctness,
#' whereas this script stresses the core functions, and looks for bottlenecks.
#' @keywords internal
icd9Benchmark <- function() {
  # generate large data set: this is copied from test-ICD9.R for now...
  set.seed(1441)
  n <- 1E7 # 10 million rows

  rpts <- randomPatients(n)

  # run slow tests (these are now much much faster with C++ implementations)
  res <- testthat::test_dir("tests/testthat/", filter = "slow", reporter = testthat::ListReporter())
  res <- as.data.frame(res)
  print(res[order(res$real), c("test", "real")])

  tmp <- tempfile(fileext = ".Rprof")
  Rprof(filename = tmp, line.profiling = TRUE, memory.profiling = TRUE)
  capture.output(icd9ComorbidAhrq(rpts, isShort = TRUE))
  Rprof(NULL)
  #summaryRprof(filename = tmp, memory = "stats", lines = "both")
  summaryRprof(filename = tmp, memory = "both", lines = "show")

  # see how we do scaling up:
  set.seed(1441)
  microbenchmark::microbenchmark(
    icd9ComorbidAhrq(randomPatients(1), isShort = TRUE),
    icd9ComorbidAhrq(randomPatients(10), isShort = TRUE),
    icd9ComorbidAhrq(randomPatients(100), isShort = TRUE),
    icd9ComorbidAhrq(randomPatients(1000), isShort = TRUE),
    # argh, we fall off a cliff between 1000 and 10000 and get much slower.
    icd9ComorbidAhrq(randomPatients(10000), isShort = TRUE),
    icd9ComorbidAhrq(randomPatients(100000), isShort = TRUE),
    times = 5
  )

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
  ggplot2::ggplot(prfChild, minlabel = 0.001)
  ggplot2::ggsave("tmpggplot.jpg", width = 250, height=5, dpi=200, limitsize = FALSE)

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
                                 icd9AddLeadingZeroesMajor(major = c(1 %i9mj% 999, paste("V", 1:9, sep=""))))

  # 3.5 sec in v0.5, 2.7 sec without validation checks
  #microbenchmark::microbenchmark(times = 5, icd9ChildrenShort_R("400" %i9s% "450"))


  #sprintf wins
  microbenchmark::microbenchmark(times = 500000, sprintf("%s%s", "410", "01"))
  microbenchmark::microbenchmark(times = 500000, paste("410", "01", sep = ""))

  #microbenchmark::microbenchmark(times = 10, icd9ShortToParts(randomShortIcd9(5E+5)))
  #microbenchmark::microbenchmark(times = 10, icd9ShortToPartsSlow(randomShortIcd9(5E+5)))
  #microbenchmark::microbenchmark(times = 10, icd9ShortToPartsList(randomShortIcd9(5E+5)))

  microbenchmark::microbenchmark(times = 50, trim(randomShortIcd9))
  microbenchmark::microbenchmark(times = 50, strip(randomShortIcd9))

  # initializing empty data frame
  microbenchmark::microbenchmark(data.frame(matrix(ncol = 2, nrow = 100000)))
  microbenchmark::microbenchmark(data.frame(major = character(100000),
                                            minor = character(100000)))

  # C++ faster, especially with multiple invocations.
  #dat <- randomShortIcd9(500)
  #microbenchmark::microbenchmark(icd9ShortToParts_R(dat), icd9ShortToParts(dat), times=5000)
  #dat <- randomShortIcd9(5000)
  #microbenchmark::microbenchmark(icd9ShortToParts_R(dat), icd9ShortToParts(dat), times=500)
  #dat <- randomShortIcd9(50000)
  #microbenchmark::microbenchmark(icd9ShortToParts_R(dat), icd9ShortToParts(dat), times=50)

  #microbenchmark::microbenchmark(icd9ShortToDecimal_R(dat), icd9ShortToDecimal(dat), times=5)

  #   major <- as.character(rep(seq.int(999), times = 5))
  #   minor <- as.character(rep(seq.int(9), times = 555))
  #   microbenchmark::microbenchmark(icd9MajMinToShort_R(major, minor), icd9MajMinToShort(major, minor),
  #                                  icd9MajMinToDecimal_R(major, minor), icd9MajMinToDecimal(major, minor),
  #                                  times=10)
  #
  #   major <- as.character(rep(seq.int(111), times = 5))
  #   minor <- as.character(rep(seq.int(1), times = 555))
  #   microbenchmark::microbenchmark(icd9MajMinToShort_R(major, minor), icd9MajMinToShort(major, minor),
  #                                  icd9MajMinToDecimal_R(major, minor), icd9MajMinToDecimal(major, minor),
  #                                  times=100)

  microbenchmark::microbenchmark(icd9ExpandMinor("7", FALSE), icd9ExpandMinor_R("7", FALSE))
  microbenchmark::microbenchmark(icd9ExpandMinor("", FALSE), icd9ExpandMinor_R("", FALSE))
  microbenchmark::microbenchmark(icd9ExpandMinor("7", TRUE), icd9ExpandMinor_R("7", TRUE))
  microbenchmark::microbenchmark(icd9ExpandMinor("", TRUE), icd9ExpandMinor_R("", TRUE))

  # regex is a little faster than fixed
  icd9 <- rep(times = 500, c("1", "not", "V10.0", " E950", ""))
  microbenchmark::microbenchmark(times = 3,
                                 grepl(pattern = "E", icd9, fixed = TRUE) |
                                   grepl(pattern = "e", icd9, fixed = TRUE) |
                                   grepl(pattern = "V", icd9, fixed = TRUE) |
                                   grepl(pattern = "v", icd9, fixed = TRUE))



  microbenchmark::microbenchmark(times = 3, grepl(pattern = "[EeVv]", rpts))
  microbenchmark::microbenchmark(icd9IsV_cpp_slower(icd9), icd9IsV_R(icd9), icd9IsV_cpp_slow(icd9), icd9IsV(icd9))

}
# EXCLUDE COVERAGE END
