# convert testthat (or I guess test-all.R) to multiple calls to test_check in the tests directory, so that CI builders don't time out, notably r-hub valgrind, and probably gcc torture if that is available.

# source from package root after setting `dry_run` to `FALSE`

testthat_active_path <- function(base_name = "testthat") {
  file.path("tests", paste0(base_name, ".R"))
}

testthat_disabled_path <- function(base_name = "testthat") {
  file.path(paste0(base_name, "-disabled.R"))
}

testthat_split <- function(base_name = "testthat",
                           filter = ".*") {
  prefix <- "testthat-split-"
  test_files <- list.files(
    file.path("tests", "testthat"),
    pattern = "^test-"
  )
  for (f in test_files) {
    if (regexec(pattern = filter, text = f, ignore.case = TRUE) == -1)
      next
    test_name <- sub(".R", "", x = sub("test-", "", x = f))
    new_test <- file.path("tests", paste0(prefix, test_name, ".R"))
    new_test_lines <- readLines(testthat_active_path())
    test_check_line_num <- grep("^ *[^#]*(testthat::)?test_check.*",
                                new_test_lines)
    test_check_line <- new_test_lines[test_check_line_num]
    test_check_line <- sub(pattern = "\\)",
                           replacement = paste0(", filter = \"^", test_name, "$\")"),
                           test_check_line)
    new_test_lines[test_check_line_num] <- test_check_line
    writeLines(new_test_lines, new_test)
  }
  ok <- file.rename(testthat_active_path(), testthat_disabled_path())
  stopifnot(ok)
  invisible()
}

testthat_restore <- function(base_name = "testthat") {
  prefix <- "testthat-split-"
  file.rename(testthat_disabled_path(), testthat_active_path())
  ok <- all(file.remove(list.files("tests", pattern = paste0(prefix),
                                   full.names = TRUE)))
  stopifnot(ok)
  invisible()
}

check_split_tests <- function(
  base_name = "testthat", # testthat is default from usethis_testthat
  valgrind = FALSE,
  torture = FALSE,
  filter = ".*",
  check_args = c("--ignore-vignettes",
                 "--no-vignettes",
                 "--no-build-vignettes",
                 "--no-manual",
                 "--timings",
                 "--no-codoc")) {
  if (!requireNamespace("rcmdcheck")) install.packages("rcmdcheck")
  if (!requireNamespace("rhub")) install.packages("rhub")
  testthat_split(base_name = base_name,
                 filter = filter)
  if (valgrind) {
    rhub::check_with_valgrind(check_args = check_args)
  } else if (torture) {
    rcmdcheck::rcmdcheck(args = c("--use-gct", check_args))
  } else {
    message("Neither valgrind or gctorture selected. Restoring testthat.R")
  }
  testthat_restore(base_name = base_name)
}
