context("condense")

test_that("five digit to five digit code range condenses", {
expect_equal(icd9Condense("34500" %i9sa% "34509", onlyReal = FALSE), "3450")
})

test_that("condensing a single real codes gives themselves", {
  expect_equal(icd9Condense("61172"), "61172")
  expect_equal(icd9Condense("61172", onlyReal = FALSE), "61172")
  expect_equal(icd9Condense("143"), "143")
  expect_equal(icd9Condense("143", onlyReal = FALSE), "143")
  expect_equal(icd9Condense("V1221"), "V1221")
  expect_equal(icd9Condense("V1221", onlyReal = FALSE), "V1221")
  expect_equal(icd9Condense("V121"), "V121")
  expect_equal(icd9Condense("V121", onlyReal = FALSE), "V121")
  expect_equal(icd9Condense("E8280"), "E8280")
  expect_equal(icd9Condense("E8280", onlyReal = FALSE), "E8280")
  expect_equal(icd9Condense("E030"), "E030")
  expect_equal(icd9Condense("E030", onlyReal = FALSE), "E030")
})

test_that("condense an ICD-9 code set to minimal group", {
  #skip("TODO:  this test breaks because %i9s% now includes the last major, even if not all its child.")
  expect_equal(sort(icd9Condense("98799" %i9sa% "98901", onlyReal = FALSE)),
               sort(c("98799", "988", "98900", "98901")))
  expect_equal(icd9Condense("98799" %i9sa% "98901", onlyReal = TRUE), "988")
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
        icd9Condense(icd9ChildrenShort("00321", onlyReal = or1), onlyReal = or2),
        "00321", info = paste("or1,2 are: ", or1, ", ", or2))
      expect_equal(
        icd9Condense(icd9ChildrenShort("V1221", onlyReal = or1), onlyReal = or2),
        "V1221", info = paste("or1,2 are: ", or1, ", ", or2))
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
