context("fast factor generation")
v1 <- c("a", "b", "a", "c")
v2 <- c("a", "b", NA, "a")
l1 <- c("b", "a", "c")
l2 <- c("a", "b")
test_that("factors are same when levels given or not", {
  for (v in list(v1, v2, NA)) {
    for (l in list(l1, l2)) {
      f1 <- factor(v, l)
      f2 <- factor_nosort(v, l)
      f3 <- factor_nosort_rcpp(v, l)
      msg <- paste(
        "v=", paste(v, collapse = ", "),
        "l=", paste(l, collapse = ", ")
      )
      expect_identical(f2, f1, info = msg)
      expect_identical(f3, f1, info = msg)
    }
  }
})
test_that("factors are same when levels given or not", {
  for (v in list(v1, v2, NA)) {
    f4 <- factor(v, exclude = "")
    f5 <- factor_nosort(v)
    f6 <- factor_nosort_rcpp(v)
    msg <- paste("v=c(\"", paste(v, collapse = "\", \""), "\")", sep = "")
    expect_identical(f5, f4, info = msg)
    expect_identical(f6, f4, info = msg)
  }
})
test_that("factors are same when levels given or not", {
  for (v in list(v1, v2, NA)) {
    f4 <- factor(v)
    f5 <- factor_nosort(v)
    f6 <- factor_nosort_rcpp(v)
    msg <- paste("v=c(\"", paste(v, collapse = "\", \""), "\")", sep = "")
    expect_identical(as.character(f5), as.character(f4), info = msg)
    expect_identical(as.character(f6), as.character(f4), info = msg)
    expect_identical(length(f5), length(f4), info = msg)
    expect_identical(length(f6), length(f4), info = msg)
  }
})

test_that("rcpp fast in and nin are ok", {
  nl <- list(
    a = c("a"),
    L = LETTERS,
    l = letters,
    na = NA_character_,
    mix = c(NA, "A", "0", "b")
  )
  for (vn in names(nl)) {
    v <- nl[[vn]]
    expect_identical(v %in% v, v %fin% v, info = vn)
    expect_identical(v %nin% v, v %fnin% v, info = vn)
    expect_identical("a" %in% v, "a" %fin% v, info = vn)
    expect_identical("a" %nin% v, "a" %fnin% v, info = vn)
    expect_identical(v %in% "a", v %fin% "a", info = vn)
    expect_identical(v %nin% "a", v %fnin% "a", info = vn)
  }
})
