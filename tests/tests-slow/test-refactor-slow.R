context("refactor bigger factors")
test_that("longer factor to touch openmp", {
  n = 1e6
  nl = 1e5
  set.seed(1441)
  expect_error(regexp = NA, v1 <- icd:::icd9RandomShort(n))
  v2 <- v1
  v2[1] <- "INVALID"
  l1 <- sample(v1, size = nl)
  l2 <- c(NA_character_, l1)
  l3 <- c(l1, NA_character_)
  l4 <- c(l1, "XXX")
  expect_error(regexp = NA, l5 <- unique(icd:::icd9RandomShort(n * 2)))
  expect_error(regexp = NA,
               test_cases <- expand.grid(
                 list(v1, v2),
                 list(l1, l2, l3, l4, l5),
                 list(l1, l2, l3, l4, l5))
  )
  for (tc in seq_along(test_cases[[1]])) {
    m <- test_cases[tc, 1][[1]]
    nl <- unique(test_cases[tc, 2][[1]])
    pl <- unique(test_cases[tc, 3][[1]])
    inf = paste(
      "tc = ", tc, "\n",
      "m = c('", paste(unlist(head(m)), collapse = "', '"), "')\n",
      "nl = c('", paste(unlist(head(nl)), collapse = "', '"), "')\n",
      "np = c('", paste(unlist(head(pl)), collapse = "', '"), "')",
      sep = ""
    )
    expect_identical(
      refactor(factor(m, levels = pl), nl),
      factor(factor(m, levels = pl), levels = nl),
      info = inf)
    expect_identical(
      refactor(factor(m, levels = pl), nl, na.rm = FALSE, exclude_na = FALSE),
      factor(factor(m, levels = pl), levels = nl, exclude = NULL),
      info = inf)
  }
})
