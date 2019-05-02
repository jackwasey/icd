context("dictionary compiles for spelling check")

test_that("dictionary created", {
  skip_on_cran()
  skip_on_appveyor()
  skip_on_travis()
  words <- .generate_spelling(save_pkg_data = FALSE)
  expect_character(words)
  expect_true(file.exists(system.file("WORDLIST", package = "icd")))
})
