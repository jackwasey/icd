context("build sysdata")
skip_slow("Skipping slow re-building of sysdata")
test_that("sysdata.rda is okay", {
  lknames <- c(
    "icd9_short_n",
    "icd9_short_v",
    "icd9_short_e",
    "icd9_short_n_defined",
    "icd9_short_v_defined",
    "icd9_short_e_defined",
    "icd9_short_n_leaf",
    "icd9_short_v_leaf",
    "icd9_short_e_leaf"
  )
  sysdat <- generate_sysdata(save_data = FALSE)
  expect_equal(names(sysdat), lknames)

  expect_lt(length(icd9_short_n_leaf$vec), length(icd9_short_n_defined$vec))
  expect_lt(length(icd9_short_v_leaf$vec), length(icd9_short_v_defined$vec))
  expect_lt(length(icd9_short_e_leaf$vec), length(icd9_short_e_defined$vec))
  expect_lt(length(icd9_short_n_defined$vec), length(icd9_short_n$vec))
  expect_lt(length(icd9_short_v_defined$vec), length(icd9_short_v$vec))
  expect_lt(length(icd9_short_e_defined$vec), length(icd9_short_e$vec))
  expect_true(all(icd9_short_n_defined$env %eine% icd9_short_n$env))
  expect_true(all(icd9_short_v_defined$env %eine% icd9_short_v$env))
  expect_true(all(icd9_short_e_defined$env %eine% icd9_short_e$env))

  expect_equal(length(icd9_short_n$env), length(icd9_short_n$vec))
  expect_equal(length(icd9_short_v$env), length(icd9_short_v$vec))
  expect_equal(length(icd9_short_e$env), length(icd9_short_e$vec))
  expect_equal(length(icd9_short_n_defined$env), length(icd9_short_n_defined$vec))
  expect_equal(length(icd9_short_v_defined$env), length(icd9_short_v_defined$vec))
  expect_equal(length(icd9_short_e_defined$env), length(icd9_short_e_defined$vec))
  expect_equal(length(icd9_short_n_leaf$env), length(icd9_short_n_leaf$vec))
  expect_equal(length(icd9_short_v_leaf$env), length(icd9_short_v_leaf$vec))
  expect_equal(length(icd9_short_e_leaf$env), length(icd9_short_e_leaf$vec))
})
