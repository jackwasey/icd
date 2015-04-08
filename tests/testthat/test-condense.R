context("condense")

test_that("condense an ICD-9 code set to minimal group", {
  #skip("TODO:  this test breaks because %i9s% now includes the last major, even if not all its child.")
  expect_equal(sort(icd9CondenseToMajorShort("98799" %i9s% "98901",
                                             onlyReal = FALSE)),
               sort(c("98799", "988", "98900", "98901")))
  expect_equal(icd9Condense("98799" %i9s% "98901",onlyReal = TRUE), "988")
  expect_equal(
    sort(icd9Condense("98799" %i9s% "98901", onlyReal = TRUE)),
    sort(
      # this list is the full range with these missing:
      #  c("9880", "9881", "9882", "9888", "9889")

      c("988", "98799", "988", "98800", "98801", "98802", "98803",
        "98804", "98805", "98806", "98807", "98808", "98809", "98810",
        "98811", "98812", "98813", "98814", "98815", "98816", "98817",
        "98818", "98819", "98820", "98821", "98822", "98823", "98824",
        "98825", "98826", "98827", "98828", "98829", "9883", "98830",
        "98831", "98832", "98833", "98834", "98835", "98836", "98837",
        "98838", "98839", "9884", "98840", "98841", "98842", "98843",
        "98844", "98845", "98846", "98847", "98848", "98849", "9885",
        "98850", "98851", "98852", "98853", "98854", "98855", "98856",
        "98857", "98858", "98859", "9886", "98860", "98861", "98862",
        "98863", "98864", "98865", "98866", "98867", "98868", "98869",
        "9887", "98870", "98871", "98872", "98873", "98874", "98875",
        "98876", "98877", "98878", "98879", "98880", "98881", "98882",
        "98883", "98884", "98885", "98886", "98887", "98888", "98889",
        "98890", "98891", "98892", "98893", "98894", "98895", "98896",
        "98897", "98898", "98899", "98900", "98901")
    ))
  # TODO: more tests
})


test_that("condense ranges which do consense", {
  expect_equal(
    icd9Condense(icd9ChildrenShort("123", onlyReal = TRUE), onlyReal = TRUE),
    "123")
  expect_equal(
    icd9Condense(icd9ChildrenShort("1", onlyReal = TRUE), onlyReal = TRUE),
    "001")
  expect_equal(icd9Condense(icd9ChildrenShort("123")), "123")
  expect_equal(icd9Condense(icd9ChildrenShort("1")), "001")
  for (or1 in c(TRUE, FALSE)) {
    for (or2 in c(TRUE, FALSE)) {
      expect_equal(
        icd9Condense(icd9ChildrenShort("00321", onlyReal = or1),
                     onlyReal = or2),
        "00321", info = paste(or1, or2))
      expect_equal(
        icd9Condense(icd9ChildrenShort("V1221", onlyReal = or1),
                     onlyReal = or2),
        "V1221", info = paste(or1, or2))
    }
  }
  expect_equal(icd9Condense(icd9ChildrenShort("V12", onlyReal = TRUE),
                            onlyReal = TRUE), "V12")
  expect_equal(icd9Condense(icd9ChildrenShort("V12", onlyReal = FALSE),
                            onlyReal = FALSE), "V12")
})

test_that("condense ranges that don't condense at all", {
  expect_equal(sort(icd9Condense(c("1230", "1232", "1236"), onlyReal = FALSE)), c("1230", "1232", "1236"))
  expect_equal(sort(icd9Condense(c("1230", "1232", "1236"), onlyReal = TRUE)), c("1230", "1232", "1236"))
  # the parent "1000" is not included.
  expect_equal(sort(icd9Condense(as.character(10000:10009),
                                 onlyReal = FALSE)),
               as.character(10000:10009))
  # missing 10009
  expect_equal(sort(icd9Condense(c("1000", as.character(10000:10008)),
                                 onlyReal = FALSE)),
               c("1000", as.character(10000:10008)))
})

test_that("condense range invalid data", {
  # no automatic validation, so we just get it back. We can validate separately.
  # e.g. "turnpike" %>% icd9GetRealShort
  expect_equal(icd9Condense("turnpike", onlyReal = FALSE), "turnpike")
  # TODO more tests here
})

test_that("mix of four and five digit billable codes", {
  expect_equal(
    icd9CondenseShort(c("10081", "10089", "1000", "1009")),
    "100")
})

test_that("mix of four and five digit with non-billable mid-level four digit code", {
  expect_equal(
    icd9CondenseShort(c("1000", "1008", "10081", "10089", "1009")),
    "100")
})
