context("function examples")

# this is essentially duplicated by R CMD check, which runs the examples, but
# doing this here allows test coverage to include more code, and perhaps to make
# sure there are no warnings thrown.

old_opts <- options(warn = 2)
on.exit(options(old_opts), add = TRUE)

testthat::test_examples()
