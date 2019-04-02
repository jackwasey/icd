context("data sanity")

pkg_ns <- asNamespace("icd")
lazy_env <- pkg_ns$.__NAMESPACE__.$lazydata

test_that("row numbers sequential for data frames", {
  for (data_name in ls(envir = lazy_env)) {
    d <- get(data_name, envir = lazy_env)
    if (!is.data.frame(d) || grepl("map", data_name)) next
    expect_gt(nrow(d), 1)
    expect_gt(ncol(d), 1)
    expect_identical(
      row.names(d),
      as.character(
        seq_along(d[[1]])
      ),
      info = paste("Data = ", data_name)
    )
    for (col in names(d)) {
      if (col == "three_digit") {
        expect_true(is.factor(d[[col]]))
        next
      }
      if (inherits(d[[col]], "icd9") ||
        inherits(d[[col]], "icd9")) {
        expect_is(d[[col]], "character",
          info = paste(
            "Data = ", data_name,
            ", Column = ", col
          )
        )
      }
    }
  }
})
