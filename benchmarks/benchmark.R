# Copyright (C) 2014 - 2018  Jack O. Wasey
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

slow_tests <- function(...) {
  r <- as.data.frame(res <- devtools::test(reporter = testthat::ListReporter))
  r[order(r$real, decreasing = T), c(1,3,11)] %>%  head(10)
  invisible(res)
}

bench_omp_vec_int <- function(n = 4, np = 2, threads = 6, chunk_size = 32) {
  icd9df <- generate_random_pts(n, np = np)
  icd9ComorbidShort(icd9df = icd9df,
                    icd9Mapping = icd9_map_ahrq,
                    visitId = get_visit_name(icd9df),
                    icd9Field = get_icd_name(icd9df),
                    threads = threads, chunk_size = chunk_size)
}

bench_long_to_wide <- function(n = 10000, np = 7, times = 10) {
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

my_check <- function(values) {
  sapply(values, function(x) message("dims: ", nrow(x), " by ", ncol(x)))
  sapply(values, function(x) message("digest: ", digest::digest(x)))
  sapply(values, function(x) {
    print(head(x)); print(tail(x))
  })
  all(sapply(values[-1], function(x) identical(values[[1]], x)))
}

#' benchmark and profile major functions with larger data sets
#'
#' \code{icd9} package is intended to be used with large data sets, with
#' millions or rows. Performance of core functions is therefore of some
#' importance, after correctness. R package test code is for correctness,
#' whereas this script stresses the core functions, and looks for bottlenecks.
#' @keywords internal
icd9_benchmark <- function() {
  # generate large data set: this is copied from test-ICD9.R for now...
  set.seed(1441)
  n <- 1E7 # 10 million rows

  rpts <- generate_random_pts(n)

  tmp <- tempfile(fileext = ".Rprof")
  utils::Rprof(filename = tmp, line.profiling = TRUE, memory.profiling = TRUE)
  utils::capture.output(icd9_comorbid_ahrq(rpts, short_code = TRUE))
  utils::Rprof(NULL)
  utils::summaryRprof(filename = tmp, memory = "both", lines = "show")

  # see how we do scaling up:
  set.seed(1441)
  microbenchmark::microbenchmark(
    comorbid_ahrq.icd9(generate_random_pts(1), short_code = TRUE),
    comorbid_ahrq.icd9(generate_random_pts(10), short_code = TRUE),
    comorbid_ahrq.icd9(generate_random_pts(100), short_code = TRUE),
    comorbid_ahrq.icd9(generate_random_pts(1000), short_code = TRUE),
    # argh, we fall off a cliff between 1000 and 10000 and get much slower.
    comorbid_ahrq.icd9(generate_random_pts(10000), short_code = TRUE),
    comorbid_ahrq.icd9(generate_random_pts(100000), short_code = TRUE),
    times = 5
  )

  tmp <- tempfile(fileext = ".Rprof")
  utils::Rprof(filename = tmp, line.profiling = TRUE, memory.profiling = FALSE)
  utils::capture.output(children.icd9("300" %i9s% "450", short_code = TRUE))
  utils::Rprof(NULL)
  utils::summaryRprof(filename = tmp, lines = "show")

  mydf <- data.frame(visitId = c("a", "b", "c"),
                     icd9 = c("441", "412.93", "004.0"),
                     stringsAsFactors = TRUE)
  prof_charl <- profr::profr(charlson(mydf,
                                      return.df = TRUE,
                                      stringsAsFactors = TRUE,
                                      short_code = FALSE))
  ggplot2::ggplot(prof_charl, minlabel = 0.04)

  rng <- "300" %i9s% "450"
  prof_child <- profr::profr(children.icd9(rng, short_code = TRUE))
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

runOpenMPVecInt <- bench_omp_vec_int # nolint
benchOpenMPThreads <- bench_omp_threads # nolint
benchLongToWide <- bench_long_to_wide # nolint
checkThreadChunk <- bench_thread_chunk # nolint

# nocov end
