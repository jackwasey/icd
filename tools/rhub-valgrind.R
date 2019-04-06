# convert testthat (or I guess test-all.R) to multiple calls to test_check in the tests directory, so that CI builders don't time out, notably r-hub valgrind, and probably gcc torture if that is available.

# source from package root after setting `dry_run` to `FALSE`

# Adjust the following as required:
base_name <- "test-all" # testthat is default from usethis_testthat
dry_run = TRUE
restore = FALSE

test_all <- file.path("tests", paste0(base_name, ".R"))
test_all_disabled <- file.path(paste0(base_name, "-disabled.R"))
prefix <- "testthat-split-"

testthat_split <- function() {
  test_files <- list.files(
    file.path("tests", "testthat"),
    pattern = "^test-"
  )
  for (f in test_files) {
    test_name <- sub(".R", "", x = sub("test-", "", x = f))
    new_test <- file.path("tests", paste0(prefix, test_name, ".R"))
    new_test_lines <- readLines(test_all)
    test_check_line_num <- grep("^ *[^#]*(testthat::)?test_check.*",
                                new_test_lines)
    test_check_line <- new_test_lines[test_check_line_num]
    test_check_line <- sub(pattern = "\\)",
                           replacement = paste0(", filter = \"^", test_name, "$\")"),
                           test_check_line)
    new_test_lines[test_check_line_num] <- test_check_line
    if (!dry_run) writeLines(new_test_lines, new_test)
  }
  if (!dry_run) ok <- file.rename(test_all, test_all_disabled)
  stopifnot(ok)
}

testthat_restore <- function() {
  file.rename(test_all_disabled, test_all)
  ok <- all(file.remove(list.files("tests", pattern = paste0(prefix),
                         full.names = TRUE)))
  stopifnot(ok)
}

if (restore) {
  testthat_restore()
} else {
  testthat_split()
  rhub::check_with_valgrind()
  testthat_restore()
}
