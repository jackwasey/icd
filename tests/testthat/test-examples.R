context("test examples as testthat test")

# this is essentially duplicated by R CMD check, which runs the examples, but
# doing this here allows test coverage to include more code.

test_examples(path = "../../man")
