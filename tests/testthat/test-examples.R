context("function examples")

# this is essentially duplicated by R CMD check, which runs the examples, but
# doing this here allows test coverage to include more code, and perhaps to make
# sure there are no warnings thrown.

old_opts <- options(warn = 2)
on.exit(options(old_opts), add = TRUE)

# avoid skip spam in tests for older testthat
skip_if_not_installed("testthat", minimum_version = "2.1")
testthat::test_examples()
