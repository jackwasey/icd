context("data fun")

test_that("download and parse generated data functions", {
  skip_slow("checking all data fetch/download/get")
  ns <- asNamespace("icd")
  for (b in c(
    .data_names,
    "icd10cm_latest"
  )) {
    inf <- paste("Data fun name:", b)
    expect_true(.exists_in_ns(.get_fetcher_name(b)), info = inf)
    if (.offline() || !.exists_in_cache(b)) {
      skip(paste(
        "Regardless of interactivity, don't download during tests and",
        inf, "is not in the cache."
      ))
    }
    f <- .get_fetcher_fun(b)
    expect_is(f(), "data.frame", info = inf)
  }
})
