context("dictionary compiles for spelling check")

test_that("dictionary created", {
  words <- generate_spelling(save_data = FALSE)
  expect_character(words)
})
