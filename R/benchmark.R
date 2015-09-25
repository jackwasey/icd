# Copyright (C) 2014 - 2015  Jack O. Wasey
#
# This file is part of icd9.
#
# icd9 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd9 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd9. If not, see <http:#www.gnu.org/licenses/>.

# EXCLUDE COVERAGE START

getSlowestTests <- function(n = 5) {
  res <- testthat::test_dir(file.path(".", "tests", "testthat"),
                            reporter = testthat::ListReporter())
  print(tail(res[order(res$real), "test"], n = n))
}

randomPatients <- function(...)
  randomOrderedPatients(...)

randomOrderedPatients <- function(...) {
  x <- randomUnorderedPatients(...)
  x[order(x$visitId), ]
}

randomUnorderedPatients <- function(num_patients = 50000, dz_per_patient = 20,
                                    n = num_patients, np = dz_per_patient) {
  set.seed(1441)
  pts <- round(n / np)
  data.frame(
    visitId = sample(seq(1, pts), replace = TRUE, size = n),
    icd9 = c(randomShortIcd9(round(n / 2)), randomShortAhrq(n - round(n / 2))),
    poa = as.factor(
      sample(x = c("Y","N", "n", "n", "y", "X", "E", "", NA),
             replace = TRUE, size = n)),
    stringsAsFactors = FALSE
  )
}

#' genereate random short icd9 codes
#' @keywords internal
#' @importFrom stats runif
randomShortIcd9 <- function(n = 50000)
  as.character(floor(stats::runif(min = 1, max = 99999, n = n)))

randomShortAhrq <- function(n = 50000)
  sample(unname(unlist(icd9::ahrqComorbid)), size = n, replace = TRUE)

randomDecimalIcd9 <- function(n = 50000)
  paste(
    round(stats::runif(min = 1, max = 999, n = n)),
    sample(icd9ExpandMinor(), replace = TRUE, size = n),
    sep = "."
  )

runOpenMPVecInt <- function(n = 4, np = 2, threads = 6, chunkSize = 32) {
  pts <- randomPatients(n, np = np)
  icd9ComorbidShortCpp(pts,icd9::ahrqComorbid, threads = threads, chunkSize = chunkSize)
}

benchOpenMPThreads <- function(n = 2 ^ 18 - 1, np = 7) {
  # if chunk size is <32 (i.e. one word) bits aren't updated correctly by concurrent threads'
  pts <- randomPatients(n, np)
  stopifnot(identical(
    icd9ComorbidShortCpp(pts, icd9::ahrqComorbid, threads = 8, chunkSize=32),
    icd9ComorbidShortCpp(pts, icd9::ahrqComorbid, threads = 8, chunkSize=32)
  ))
  stopifnot(identical(
    icd9ComorbidShortCpp(pts, icd9::ahrqComorbid, threads = 1, chunkSize=32),
    icd9ComorbidShortCpp(pts, icd9::ahrqComorbid, threads = 8, chunkSize=32)
  ))
  microbenchmark::microbenchmark(
    icd9ComorbidShortCpp(pts, icd9::ahrqComorbid, threads = 4, chunkSize=32),
    icd9ComorbidShortCpp(pts, icd9::ahrqComorbid, threads = 4, chunkSize=256),
    icd9ComorbidShortCpp(pts, icd9::ahrqComorbid, threads = 4, chunkSize=1024),
    times = 5
  )
}

benchVaryn <- function(np = 5, threads = 4, chunkSize = 256, ompChunkSize = 1) {
  # default to good options from other tests
  mbr <- NULL
  for (n in c(1e0, 1e3, 1e4, 1e5, 1e6)) {
    #for (n in c(1e0, 1e3)) {
    message("n = ", n)
    pts <- randomOrderedPatients(n, np)
    res <- microbenchmark::microbenchmark(
      icd9ComorbidShortCpp(pts,icd9::ahrqComorbid, threads = 4, chunkSize = chunkSize, ompChunkSize = ompChunkSize),
      times = 5)
    if (is.null(mbr))
      mbr <- cbind(n,res)
    else
      mbr <- rbind(mbr, cbind(n,res))
  }
  mbr
}

otherbench <- function() {

  # explore the parameter space. TODO: genetic optimize these parameters (and
  # this may uncover corner case bugs, too)

  # vary threads for big n, chunk = 1
  microbenchmark::microbenchmark(
    icd9ComorbidShortCpp(randomPatients(1000000),icd9::ahrqComorbid, threads = 1, chunkSize=1),
    icd9ComorbidShortCpp(randomPatients(1000000),icd9::ahrqComorbid, threads = 2, chunkSize=1),
    icd9ComorbidShortCpp(randomPatients(1000000),icd9::ahrqComorbid, threads = 4, chunkSize=1),
    icd9ComorbidShortCpp(randomPatients(1000000),icd9::ahrqComorbid, threads = 6, chunkSize=1),
    icd9ComorbidShortCpp(randomPatients(1000000),icd9::ahrqComorbid, threads = 8, chunkSize=1),
    times = 5
  )
  # vary threads for big n, chunk = 256
  microbenchmark::microbenchmark(
    icd9ComorbidShortCpp(randomPatients(1000000),icd9::ahrqComorbid, threads = 1, chunkSize=256),
    icd9ComorbidShortCpp(randomPatients(1000000),icd9::ahrqComorbid, threads = 2, chunkSize=256),
    icd9ComorbidShortCpp(randomPatients(1000000),icd9::ahrqComorbid, threads = 4, chunkSize=256),
    icd9ComorbidShortCpp(randomPatients(1000000),icd9::ahrqComorbid, threads = 6, chunkSize=256),
    icd9ComorbidShortCpp(randomPatients(1000000),icd9::ahrqComorbid, threads = 8, chunkSize=256),
    times = 5
  )
  fivemillion <- randomPatients(5000000, 5);
  microbenchmark::microbenchmark(
    icd9ComorbidShortCpp(fivemillion,icd9::ahrqComorbid, threads = 4, chunkSize=32, ompChunkSize = 1),
    icd9ComorbidShortCpp(fivemillion,icd9::ahrqComorbid, threads = 4, chunkSize=256, ompChunkSize = 1),
    icd9ComorbidShortCpp(fivemillion,icd9::ahrqComorbid, threads = 4, chunkSize=4096, ompChunkSize = 1),
    icd9ComorbidShortCpp(fivemillion,icd9::ahrqComorbid, threads = 4, chunkSize=32, ompChunkSize = 4),
    # next row barely won with 1e6*5 rows
    icd9ComorbidShortCpp(fivemillion,icd9::ahrqComorbid, threads = 4, chunkSize=256, ompChunkSize = 4),
    icd9ComorbidShortCpp(fivemillion,icd9::ahrqComorbid, threads = 4, chunkSize=4096, ompChunkSize = 4),
    icd9ComorbidShortCpp(fivemillion,icd9::ahrqComorbid, threads = 4, chunkSize=32, ompChunkSize = 8),
    icd9ComorbidShortCpp(fivemillion,icd9::ahrqComorbid, threads = 4, chunkSize=256, ompChunkSize = 8),
    icd9ComorbidShortCpp(fivemillion,icd9::ahrqComorbid, threads = 4, chunkSize=4096, ompChunkSize = 8),
    times = 5
  )
  microbenchmark::microbenchmark(
    icd9ComorbidShortCpp(fivemillion,icd9::ahrqComorbid, threads = 4, chunkSize=256, ompChunkSize = 2),
    icd9ComorbidShortCpp(fivemillion,icd9::ahrqComorbid, threads = 4, chunkSize=256, ompChunkSize = 4),
    icd9ComorbidShortCpp(fivemillion,icd9::ahrqComorbid, threads = 4, chunkSize=256, ompChunkSize = 8),
    icd9ComorbidShortCpp(fivemillion,icd9::ahrqComorbid, threads = 8, chunkSize=256, ompChunkSize = 2),
    icd9ComorbidShortCpp(fivemillion,icd9::ahrqComorbid, threads = 8, chunkSize=256, ompChunkSize = 4),
    icd9ComorbidShortCpp(fivemillion,icd9::ahrqComorbid, threads = 8, chunkSize=256, ompChunkSize = 8),
    times = 25
  )
  # with parallel for, best is 8 threads, static chunk of ONE (dynamic slightly slower)
  for (threads in c(1,4,8)) {
    for (n in c(500000)) {
      for (cs in c(1, 32, 1024)) {
        message("threads = ", threads, ", n = ", n, ", cs = ", cs)
        print(microbenchmark::microbenchmark(
          icd9ComorbidShortCpp(randomPatients(n),icd9::ahrqComorbid, threads = threads, chunkSize = cs),
          check = my_check, times = 10
        ))
      }
    }
  }
  stopifnot(identical(
    icd9ComorbidShortCpp(randomPatients(n),icd9::ahrqComorbid, threads = 4, chunkSize=1),
    icd9ComorbidShortCpp(randomPatients(n),icd9::ahrqComorbid, threads = 4, chunkSize=32)
  ))
  stopifnot(identical(
    icd9ComorbidShortCpp(randomPatients(n),icd9::ahrqComorbid, threads = 1, chunkSize=1),
    icd9ComorbidShortCpp(randomPatients(n),icd9::ahrqComorbid, threads = 4, chunkSize=1)
  ))
  stopifnot(identical(
    icd9ComorbidShortCpp(randomPatients(n),icd9::ahrqComorbid, threads = 1, chunkSize=32),
    icd9ComorbidShortCpp(randomPatients(n),icd9::ahrqComorbid, threads = 4, chunkSize=32)
  ))
}

benchLongToWide <- function(n = 10000, np = 7, times = 10) {
  pts <- randomOrderedPatients(n, np)
  #   microbenchmark::microbenchmark(icd9LongToWideMatrixByMap(pts),
  #                  icd9LongToWideMatrixAggregate(pts),
  #                  icd9LongToWideMatrixNoAggregate(pts),
  #                  times = times)
  microbenchmark::microbenchmark(icd9LongToWide(pts, aggregate = FALSE),
                                 icd9LongToWide(pts, aggregate = TRUE),
                                 times = times
  )
}

checkThreadChunk <- function() {
  for (n in c(1, 12345)) {
    for (np in c(1, 30)) {
      pts <- randomPatients(n, np);
      message("NOT BENCHMARKING HERE, just checking. np = ", np, ", n = ", n)
      # use microbenchmark::microbenchmark to conveniently check the results are all identical
      microbenchmark::microbenchmark(
        icd9ComorbidShortCpp(pts,icd9::ahrqComorbid, threads = 1, chunkSize = 1, ompChunkSize = 1),
        icd9ComorbidShortCpp(pts,icd9::ahrqComorbid, threads = 1, chunkSize = 1, ompChunkSize = 1024),
        icd9ComorbidShortCpp(pts,icd9::ahrqComorbid, threads = 1, chunkSize = 1024, ompChunkSize = 1),
        icd9ComorbidShortCpp(pts,icd9::ahrqComorbid, threads = 1, chunkSize = 1024, ompChunkSize = 1024),
        icd9ComorbidShortCpp(pts,icd9::ahrqComorbid, threads = 8, chunkSize = 1, ompChunkSize = 1),
        icd9ComorbidShortCpp(pts,icd9::ahrqComorbid, threads = 8, chunkSize = 1, ompChunkSize = 1024),
        icd9ComorbidShortCpp(pts,icd9::ahrqComorbid, threads = 8, chunkSize = 1024, ompChunkSize = 1),
        icd9ComorbidShortCpp(pts,icd9::ahrqComorbid, threads = 8, chunkSize = 1024, ompChunkSize = 1024),
        check = my_check, times = 1)
    }
  }
}

my_check <- function(values) {
  sapply(values, function(x) message("dims: ", nrow(x), " by ", ncol(x)))
  sapply(values, function(x) message("digest: ", digest::digest(x)))
  sapply(values, function(x) {
    print(utils::head(x)); print(utils::tail(x))
  })
  all(sapply(values[-1], function(x) identical(values[[1]], x)))
}

icd9BenchComorbidParallel <- function() {
  pts10000 <- randomPatients(10000)
  pts100000 <- randomPatients(100000)
  ptsBig <- randomPatients(500000)
  print(microbenchmark::microbenchmark(
    icd9ComorbidShortCpp(pts10000,icd9::ahrqComorbid, threads = 0),
    icd9ComorbidShortCpp(pts10000,icd9::ahrqComorbid, threads = 1),
    icd9ComorbidShortCpp(pts10000,icd9::ahrqComorbid, threads = 2),
    icd9ComorbidShortCpp(pts10000,icd9::ahrqComorbid, threads = 4),
    icd9ComorbidShortCpp(pts10000,icd9::ahrqComorbid, threads = 6),
    icd9ComorbidShortCpp(pts100000,icd9::ahrqComorbid, threads = 0),
    icd9ComorbidShortCpp(pts100000,icd9::ahrqComorbid, threads = 1),
    icd9ComorbidShortCpp(pts100000,icd9::ahrqComorbid, threads = 2),
    icd9ComorbidShortCpp(pts100000,icd9::ahrqComorbid, threads = 4),
    icd9ComorbidShortCpp(pts100000,icd9::ahrqComorbid, threads = 6),
    icd9ComorbidShortCpp(ptsBig,icd9::ahrqComorbid, threads = 0),
    icd9ComorbidShortCpp(ptsBig,icd9::ahrqComorbid, threads = 1),
    icd9ComorbidShortCpp(ptsBig,icd9::ahrqComorbid, threads = 2),
    icd9ComorbidShortCpp(ptsBig,icd9::ahrqComorbid, threads = 4),
    icd9ComorbidShortCpp(ptsBig,icd9::ahrqComorbid, threads = 6),
    # 8 is very slow (maxes hyperthreading...)
    times = 5))
}

#' benchmark and profile major functions with larger data sets
#'
#' \code{icd9} package is intended to be used with large data sets, with
#' millions or rows. Performance of core functions is therefore of some
#' importance, after correctness. R package test code is for correctness,
#' whereas this script stresses the core functions, and looks for bottlenecks.
#' @keywords internal
#' @importFrom utils Rprof capture.output Rprof summaryRprof
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
  utils::Rprof(filename = tmp, line.profiling = TRUE, memory.profiling = TRUE)
  utils::capture.output(icd9ComorbidAhrq(rpts, isShort = TRUE))
  utils::Rprof(NULL)
  utils::summaryRprof(filename = tmp, memory = "both", lines = "show")

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
  utils::Rprof(filename = tmp, line.profiling = TRUE, memory.profiling = FALSE)
  utils::capture.output(icd9ChildrenShort("300" %i9s% "450"))
  utils::Rprof(NULL)
  utils::summaryRprof(filename = tmp, lines = "show")

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

  microbenchmark::microbenchmark(times = 500, # initial about 2ms
                                 icd9AddLeadingZeroesMajor(major = c(1 %i9mj% 999, paste("V", 1:9, sep = ""))))

  microbenchmark::microbenchmark(times = 500, # initial about 2ms
                                 icd9AddLeadingZeroesMajor(major = c(1 %i9mj% 999, paste("V", 1:9, sep = ""))))

  #sprintf wins
  microbenchmark::microbenchmark(times = 500000, sprintf("%s%s", "410", "01"))
  microbenchmark::microbenchmark(times = 500000, paste("410", "01", sep = ""))
  microbenchmark::microbenchmark(times = 50, trim(randomShortIcd9))
  microbenchmark::microbenchmark(times = 50, strip(randomShortIcd9))

  # initializing empty data frame
  microbenchmark::microbenchmark(data.frame(matrix(ncol = 2, nrow = 100000)))
  microbenchmark::microbenchmark(data.frame(major = character(100000),
                                            minor = character(100000)))
  # regex is a little faster than fixed
  icd9 <- rep(times = 500, c("1", "not", "V10.0", " E950", ""))
  microbenchmark::microbenchmark(times = 3,
                                 grepl(pattern = "E", icd9, fixed = TRUE) |
                                   grepl(pattern = "e", icd9, fixed = TRUE) |
                                   grepl(pattern = "V", icd9, fixed = TRUE) |
                                   grepl(pattern = "v", icd9, fixed = TRUE))

  microbenchmark::microbenchmark(times = 3, grepl(pattern = "[EeVv]", rpts))
}
# EXCLUDE COVERAGE END
