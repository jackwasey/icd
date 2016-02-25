# Copyright (C) 2014 - 2016  Jack O. Wasey
#
# This file is part of icd.
#
# icd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd. If not, see <http:#www.gnu.org/licenses/>.

# nocov start

runOpenMPVecInt <- function(n = 4, np = 2, threads = 6, chunk_size = 32) {
  icd9df <- generate_random_pts(n, np = np)
  icd9ComorbidShort(icd9df = icd9df,
                    icd9Mapping = icd::icd9_map_ahrq,
                    visitId = get_visit_name(icd9df),
                    icd9Field = get_icd_name(icd9df),
                    threads = threads, chunk_size = chunk_size)
}

benchOpenMPThreads <- function(n = 2 ^ 18 - 1, np = 7) {
  # if chunk size is <32 (i.e. one word) bits aren't updated correctly by concurrent threads'
  pts <- generate_random_pts(n, np)
  stopifnot(identical(
    icd9ComorbidShortCpp(pts, icd::icd9_map_ahrq, visitId = get_visit_name(pts),
                         icd9Field = get_icd_name(pts), threads = 8, chunk_size = 32),
    icd9ComorbidShortCpp(pts, icd::icd9_map_ahrq, visitId = get_visit_name(pts),
                         icd9Field = get_icd_name(pts), threads = 8, chunk_size = 32)
  ))
  stopifnot(identical(
    icd9ComorbidShortCpp(pts, icd::icd9_map_ahrq, visitId = get_visit_name(pts),
                         icd9Field = get_icd_name(pts), threads = 1, chunk_size = 32),
    icd9ComorbidShortCpp(pts, icd::icd9_map_ahrq, visitId = get_visit_name(pts),
                         icd9Field = get_icd_name(pts), threads = 8, chunk_size = 32)
  ))
  microbenchmark::microbenchmark(
    icd9ComorbidShortCpp(pts, icd::icd9_map_ahrq, visitId = get_visit_name(pts),
                         icd9Field = get_icd_name(pts), threads = 4, chunk_size = 32),
    icd9ComorbidShortCpp(pts, icd::icd9_map_ahrq, visitId = get_visit_name(pts),
                         icd9Field = get_icd_name(pts), threads = 4, chunk_size = 256),
    icd9ComorbidShortCpp(pts, icd::icd9_map_ahrq, visitId = get_visit_name(pts),
                         icd9Field = get_icd_name(pts), threads = 4, chunk_size = 1024),
    times = 5
  )
}

benchVaryn <- function(np = 5, threads = 4, chunk_size = 256, omp_chunk_size = 1) {
  # default to good options from other tests
  mbr <- NULL
  for (n in c(1e0, 1e3, 1e4, 1e5, 1e6)) {
    #for (n in c(1e0, 1e3)) {
    message("n = ", n)
    pts <- icd::generate_random_ordered_pts(n, np)
    res <- microbenchmark::microbenchmark(
      icd9ComorbidShortCpp(pts, icd::icd9_map_ahrq,
                           visitId = get_visit_name(pts),
                           icd9Field = get_icd_name(pts),
                           threads = 4,
                           chunk_size = chunk_size,
                           omp_chunk_size = omp_chunk_size),
      times = 5)
    if (is.null(mbr))
      mbr <- cbind(n, res)
    else
      mbr <- rbind(mbr, cbind(n, res))
  }
  mbr
}

otherbench <- function() {

  # Someday explore the parameter space possibly with genetic optimization.

  # vary threads for big n, chunk = 1
  microbenchmark::microbenchmark(
    icd9ComorbidShort(generate_random_pts(1000000), icd::icd9_map_ahrq, threads = 1, chunk_size = 1),
    icd9ComorbidShort(generate_random_pts(1000000), icd::icd9_map_ahrq, threads = 2, chunk_size = 1),
    icd9ComorbidShort(generate_random_pts(1000000), icd::icd9_map_ahrq, threads = 4, chunk_size = 1),
    icd9ComorbidShort(generate_random_pts(1000000), icd::icd9_map_ahrq, threads = 6, chunk_size = 1),
    icd9ComorbidShort(generate_random_pts(1000000), icd::icd9_map_ahrq, threads = 8, chunk_size = 1),
    times = 5
  )
  # vary threads for big n, chunk = 256
  microbenchmark::microbenchmark(
    icd9ComorbidShort(generate_random_pts(1000000), icd::icd9_map_ahrq, threads = 1, chunk_size = 256),
    icd9ComorbidShort(generate_random_pts(1000000), icd::icd9_map_ahrq, threads = 2, chunk_size = 256),
    icd9ComorbidShort(generate_random_pts(1000000), icd::icd9_map_ahrq, threads = 4, chunk_size = 256),
    icd9ComorbidShort(generate_random_pts(1000000), icd::icd9_map_ahrq, threads = 6, chunk_size = 256),
    icd9ComorbidShort(generate_random_pts(1000000), icd::icd9_map_ahrq, threads = 8, chunk_size = 256),
    times = 5
  )
  fivemillion <- generate_random_pts(5000000, 5);
  microbenchmark::microbenchmark(
    icd9ComorbidShort(fivemillion, icd::icd9_map_ahrq, threads = 4, chunk_size = 32, omp_chunk_size = 1),
    icd9ComorbidShort(fivemillion, icd::icd9_map_ahrq, threads = 4, chunk_size = 256, omp_chunk_size = 1),
    icd9ComorbidShort(fivemillion, icd::icd9_map_ahrq, threads = 4, chunk_size = 4096, omp_chunk_size = 1),
    icd9ComorbidShort(fivemillion, icd::icd9_map_ahrq, threads = 4, chunk_size = 32, omp_chunk_size = 4),
    # next row barely won with 1e6*5 rows
    icd9ComorbidShort(fivemillion, icd::icd9_map_ahrq, threads = 4, chunk_size = 256, omp_chunk_size = 4),
    icd9ComorbidShort(fivemillion, icd::icd9_map_ahrq, threads = 4, chunk_size = 4096, omp_chunk_size = 4),
    icd9ComorbidShort(fivemillion, icd::icd9_map_ahrq, threads = 4, chunk_size = 32, omp_chunk_size = 8),
    icd9ComorbidShort(fivemillion, icd::icd9_map_ahrq, threads = 4, chunk_size = 256, omp_chunk_size = 8),
    icd9ComorbidShort(fivemillion, icd::icd9_map_ahrq, threads = 4, chunk_size = 4096, omp_chunk_size = 8),
    times = 5
  )
  microbenchmark::microbenchmark(
    icd9ComorbidShort(fivemillion, icd::icd9_map_ahrq, threads = 4, chunk_size = 256, omp_chunk_size = 2),
    icd9ComorbidShort(fivemillion, icd::icd9_map_ahrq, threads = 4, chunk_size = 256, omp_chunk_size = 4),
    icd9ComorbidShort(fivemillion, icd::icd9_map_ahrq, threads = 4, chunk_size = 256, omp_chunk_size = 8),
    icd9ComorbidShort(fivemillion, icd::icd9_map_ahrq, threads = 8, chunk_size = 256, omp_chunk_size = 2),
    icd9ComorbidShort(fivemillion, icd::icd9_map_ahrq, threads = 8, chunk_size = 256, omp_chunk_size = 4),
    icd9ComorbidShort(fivemillion, icd::icd9_map_ahrq, threads = 8, chunk_size = 256, omp_chunk_size = 8),
    times = 25
  )
  # with parallel for, best is 8 threads, static chunk of ONE (dynamic slightly slower)
  for (threads in c(1, 4, 8)) {
    for (n in c(500000)) {
      for (cs in c(1, 32, 1024)) {
        message("threads = ", threads, ", n = ", n, ", cs = ", cs)
        print(microbenchmark::microbenchmark(
          icd9ComorbidShort(generate_random_pts(n), icd::icd9_map_ahrq, threads = threads, chunk_size = cs),
          check = my_check, times = 10
        ))
      }
    }
  }
  stopifnot(identical(
    icd9ComorbidShort(generate_random_pts(n), icd::icd9_map_ahrq, threads = 4, chunk_size = 1),
    icd9ComorbidShort(generate_random_pts(n), icd::icd9_map_ahrq, threads = 4, chunk_size = 32)
  ))
  stopifnot(identical(
    icd9ComorbidShort(generate_random_pts(n), icd::icd9_map_ahrq, threads = 1, chunk_size = 1),
    icd9ComorbidShort(generate_random_pts(n), icd::icd9_map_ahrq, threads = 4, chunk_size = 1)
  ))
  stopifnot(identical(
    icd9ComorbidShort(generate_random_pts(n), icd::icd9_map_ahrq, threads = 1, chunk_size = 32),
    icd9ComorbidShort(generate_random_pts(n), icd::icd9_map_ahrq, threads = 4, chunk_size = 32)
  ))
}

benchLongToWide <- function(n = 10000, np = 7, times = 10) {
  pts <- generate_random_ordered_pts(n, np)
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
      pts <- generate_random_pts(n, np);
      message("NOT BENCHMARKING HERE, just checking. np = ", np, ", n = ", n)
      # use microbenchmark::microbenchmark to conveniently check the results are all identical
      microbenchmark::microbenchmark(
        icd9ComorbidShortCpp(pts, icd::icd9_map_ahrq, visitId = "visitId", icd9Field = "code",
                             threads = 1, chunk_size = 1, omp_chunk_size = 1),
        icd9ComorbidShortCpp(pts, icd::icd9_map_ahrq, visitId = "visitId", icd9Field = "code",
                             threads = 1, chunk_size = 1, omp_chunk_size = 1024),
        icd9ComorbidShortCpp(pts, icd::icd9_map_ahrq, visitId = "visitId", icd9Field = "code",
                             threads = 1, chunk_size = 1024, omp_chunk_size = 1),
        icd9ComorbidShortCpp(pts, icd::icd9_map_ahrq, visitId = "visitId", icd9Field = "code",
                             threads = 1, chunk_size = 1024, omp_chunk_size = 1024),
        icd9ComorbidShortCpp(pts, icd::icd9_map_ahrq, visitId = "visitId", icd9Field = "code",
                             threads = 8, chunk_size = 1, omp_chunk_size = 1),
        icd9ComorbidShortCpp(pts, icd::icd9_map_ahrq, visitId = "visitId", icd9Field = "code",
                             threads = 8, chunk_size = 1, omp_chunk_size = 1024),
        icd9ComorbidShortCpp(pts, icd::icd9_map_ahrq, visitId = "visitId", icd9Field = "code",
                             threads = 8, chunk_size = 1024, omp_chunk_size = 1),
        icd9ComorbidShortCpp(pts, icd::icd9_map_ahrq, visitId = "visitId", icd9Field = "code",
                             threads = 8, chunk_size = 1024, omp_chunk_size = 1024),
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
  pts10000 <- generate_random_pts(10000)
  pts100000 <- generate_random_pts(100000)
  pts_big <- generate_random_pts(500000)
  print(microbenchmark::microbenchmark(
    icd9ComorbidShortCpp(pts10000, visitId = "visitId", icd9Field = "code", icd::icd9_map_ahrq, threads = 0),
    icd9ComorbidShortCpp(pts10000, visitId = "visitId", icd9Field = "code", icd::icd9_map_ahrq, threads = 1),
    icd9ComorbidShortCpp(pts10000, visitId = "visitId", icd9Field = "code", icd::icd9_map_ahrq, threads = 2),
    icd9ComorbidShortCpp(pts10000, visitId = "visitId", icd9Field = "code", icd::icd9_map_ahrq, threads = 4),
    icd9ComorbidShortCpp(pts10000, visitId = "visitId", icd9Field = "code", icd::icd9_map_ahrq, threads = 6),
    icd9ComorbidShortCpp(pts100000, visitId = "visitId", icd9Field = "code", icd::icd9_map_ahrq, threads = 0),
    icd9ComorbidShortCpp(pts100000, visitId = "visitId", icd9Field = "code", icd::icd9_map_ahrq, threads = 1),
    icd9ComorbidShortCpp(pts100000, visitId = "visitId", icd9Field = "code", icd::icd9_map_ahrq, threads = 2),
    icd9ComorbidShortCpp(pts100000, visitId = "visitId", icd9Field = "code", icd::icd9_map_ahrq, threads = 4),
    icd9ComorbidShortCpp(pts100000, visitId = "visitId", icd9Field = "code", icd::icd9_map_ahrq, threads = 6),
    icd9ComorbidShortCpp(pts_big, visitId = "visitId", icd9Field = "code", icd::icd9_map_ahrq, threads = 0),
    icd9ComorbidShortCpp(pts_big, visitId = "visitId", icd9Field = "code", icd::icd9_map_ahrq, threads = 1),
    icd9ComorbidShortCpp(pts_big, visitId = "visitId", icd9Field = "code", icd::icd9_map_ahrq, threads = 2),
    icd9ComorbidShortCpp(pts_big, visitId = "visitId", icd9Field = "code", icd::icd9_map_ahrq, threads = 4),
    icd9ComorbidShortCpp(pts_big, visitId = "visitId", icd9Field = "code", icd::icd9_map_ahrq, threads = 6),
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
icd9Benchmark <- function() {
  # generate large data set: this is copied from test-ICD9.R for now...
  set.seed(1441)
  n <- 1E7 # 10 million rows

  rpts <- generate_random_pts(n)

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
    icd9ComorbidAhrq(generate_random_pts(1), isShort = TRUE),
    icd9ComorbidAhrq(generate_random_pts(10), isShort = TRUE),
    icd9ComorbidAhrq(generate_random_pts(100), isShort = TRUE),
    icd9ComorbidAhrq(generate_random_pts(1000), isShort = TRUE),
    # argh, we fall off a cliff between 1000 and 10000 and get much slower.
    icd9ComorbidAhrq(generate_random_pts(10000), isShort = TRUE),
    icd9ComorbidAhrq(generate_random_pts(100000), isShort = TRUE),
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
  prof_charl <- profr::profr(icd9Charlson(mydf,
                                        return.df = TRUE,
                                        stringsAsFactors = TRUE,
                                        isShort = FALSE))
  ggplot2::ggplot(prof_charl, minlabel = 0.04)

  rng <- "300" %i9s% "450"
  prof_child <- profr::profr(icd9ChildrenShort(rng))
  ggplot2::ggplot(prof_child, minlabel = 0.001)
  ggplot2::ggsave("tmpggplot.jpg", width = 250, height = 5, dpi = 200, limitsize = FALSE)

  microbenchmark::microbenchmark(times = 500, # initial about 2ms
                                 icd9AddLeadingZeroesMajor(c(1 %i9mj% 999, paste("V", 1:9, sep = ""))))

  microbenchmark::microbenchmark(times = 500, # initial about 2ms
                                 icd9AddLeadingZeroesMajor(c(1 %i9mj% 999, paste("V", 1:9, sep = ""))))

  #sprintf wins
  microbenchmark::microbenchmark(times = 500000, sprintf("%s%s", "410", "01"))
  microbenchmark::microbenchmark(times = 500000, paste("410", "01", sep = ""))
  rsicd9 <- generate_random_short_icd9()
  microbenchmark::microbenchmark(times = 50, trim(rsicd9))
  microbenchmark::microbenchmark(times = 50, strip(rsicd9))

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
# nocov end
