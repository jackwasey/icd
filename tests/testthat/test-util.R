context("test utility functions")

test_that('strMultiMatch with and without dropping empties', {

  expect_equal(strMultiMatch(pattern="jack", text=c("not", "here")), list(character(), character()))
  expect_equal(strMultiMatch(pattern="(jack)", text=c("not", "here")), list(character(), character()))
  expect_equal(strMultiMatch(pattern="(jack)(second)", text=c("not", "here")), list(character(), character()))

  expect_equal(strMultiMatch(pattern="jack", text=c("not", "here"), dropEmpty = TRUE), list())
  expect_equal(strMultiMatch(pattern="(jack)", text=c("not", "here"), dropEmpty = TRUE), list())
  expect_equal(strMultiMatch(pattern="(jack)(second)", text=c("not", "here"), dropEmpty = TRUE), list())

  expect_equal(strMultiMatch("LET (jack)(w)", c("LET jack", "LET jackw", "nothing", "LET else"), dropEmpty = TRUE),
               list(c("jack", "w")))
})

