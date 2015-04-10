context("condense")

test_that("five digit to five digit code range condenses", {
expect_equal(icd9Condense("34500" %i9sa% "34509", onlyReal = FALSE), "3450")
})

test_that("condensing a single real codes gives themselves", {
  expect_that(res <- icd9Condense("61172"), shows_message())
  expect_equal(res, "61172")
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
  # non-real end of real range
  expect_that(res <- icd9Condense("988" %i9sa% "98899", onlyReal = TRUE), gives_warning())
  expect_equal(res, "988")
  expect_that(res <- icd9Condense("9879" %i9sa% "9891", onlyReal = TRUE), gives_warning())
  expect_equal(res, c("9879", "988", "9890", "9891"))

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
  expect_equal(icd9Condense("turnpike", onlyReal = FALSE), character(0))
  # TODO more tests here
})

test_that("mix of four and five digit billable codes", {
  # this is all of leptospirosis, but missing the "1008" non-billable sub-heading
  expect_equal(
    icd9CondenseShort(c("1000", "10081", "10089", "1009")),
    "100")
})

test_that("mix of four and five digit billable codes over bigger range", {
  # this is all of leptospirosis, but missing the "1008" non-billable sub-heading
  expect_equal(
    icd9CondenseShort(c("1000", "10081", "10089", "1009", "101")),
    c("100", "101"))
})


test_that("mix of four and five digit with non-billable mid-level four digit code", {
  expect_equal(
    icd9CondenseShort(c("1000", "1008", "10081", "10089", "1009")),
    "100")
})


test_that("condense short range", {

  expect_equal(icd9ExplainShort(icd9Short = othersalmonella),
               "Other salmonella infections")

  expect_equal(icd9CondenseShort(othersalmonella, onlyReal = TRUE), "003")
  expect_that(res <- icd9CondenseShort(othersalmonella, onlyReal = FALSE), testthat::not(gives_warning()))
  expect_equal(res, othersalmonella)
  # missing this leaf node, we can't condense at all
  expect_equal(icd9CondenseShort(othersalmonella[-3], onlyReal = TRUE),
               othersalmonella[-3])
  # if we demand condensing to all possible values, we get the same back
  expect_equal(icd9CondenseShort(othersalmonella[-3], onlyReal = FALSE),
               othersalmonella[-3])

  expect_equal(sort(icd9ChildrenShort(icd9Short = "001", onlyBillable = TRUE)),
               c("0010", "0011", "0019"))

  expect_equal(sort(icd9ChildrenShort(icd9Short = "001", onlyReal = TRUE)),
               c("001", "0010", "0011", "0019"))

  expect_equal(icd9CondenseShort(icd9ChildrenShort("00320", onlyReal = TRUE), onlyReal = TRUE), "00320")
  # majors should be okay, even if not 'real'
  expect_that(dup_res <- icd9CondenseShort(icd9ChildrenShort("003", onlyReal = TRUE)),
              testthat::not(gives_warning()))

  expect_equal(icd9CondenseShort(c("003", "003"), onlyReal = TRUE), "003")
  expect_equal(icd9CondenseShort(c("003", "003"), onlyReal = FALSE), "003")
})

test_that("condense full ranges", {
  # condensing to "real" means we don't get a lot of majors, which are often not
  # themselves defined.
  # majors:
  expect_equal(icd9CondenseShort(icd9ChildrenShort("003", onlyReal = FALSE), onlyReal = FALSE), "003")
  expect_equal(icd9CondenseShort(icd9ChildrenShort("3", onlyReal = FALSE), onlyReal = FALSE), "003")
  expect_equal(icd9CondenseShort(icd9ChildrenShort("410", onlyReal = FALSE), onlyReal = FALSE), "410")
  expect_equal(icd9CondenseShort(icd9ChildrenShort("V12", onlyReal = FALSE), onlyReal = FALSE), "V12")
  expect_equal(icd9CondenseShort(icd9ChildrenShort("E800", onlyReal = FALSE), onlyReal = FALSE), "E800")
  # repeat some tests with decimals instead
  expect_equal(icd9CondenseDecimal(icd9Children("003", isShort = FALSE, onlyReal = FALSE), onlyReal = FALSE), "003")
  expect_equal(icd9Condense(icd9ChildrenDecimal("3", onlyReal = FALSE), isShort = FALSE, onlyReal = FALSE), "003")
  expect_equal(icd9CondenseDecimal(icd9ChildrenDecimal("410", onlyReal = FALSE), onlyReal = FALSE), "410")
  expect_equal(icd9CondenseDecimal(icd9Children("V12", isShort = FALSE, onlyReal = FALSE), onlyReal = FALSE), "V12")
  expect_equal(icd9CondenseDecimal(icd9ChildrenDecimal("E800", onlyReal = FALSE), onlyReal = FALSE), "E800")
  # repeat some tests with decimals and smaller codes
  expect_equal(icd9CondenseDecimal(icd9Children("003.2", isShort = FALSE, onlyReal = FALSE), onlyReal = FALSE),
               "003.2")
  expect_equal(icd9Condense(icd9ChildrenDecimal("3.2", onlyReal = FALSE), isShort = FALSE, onlyReal = FALSE),
               "003.2")
  expect_equal(icd9CondenseDecimal(icd9ChildrenDecimal("410.0", onlyReal = FALSE), onlyReal = FALSE), "410.0")
  expect_equal(icd9CondenseDecimal(icd9Children("V12", isShort = FALSE, onlyReal = FALSE), onlyReal = FALSE), "V12")
  expect_equal(icd9CondenseDecimal(icd9ChildrenDecimal("E800", onlyReal = FALSE), onlyReal = FALSE), "E800")

  expect_equal(icd9CondenseShort(icd9ChildrenShort("0031", onlyReal = FALSE), onlyReal = FALSE), "0031")
  # major is alloect_equal(icd9CondenseShort(c("003", othersalmonella), onlyReal = TRUE), "003")
  # major is retupect_equal(icd9CondenseShort(othersalmonella, onlyReal = TRUE), "003")
  expect_equal(icd9CondenseShort(othersalmonella, onlyReal = FALSE), othersalmonella)
  # now do we fining major if all chilren present?
  almostall003 <- icd9ChildrenShort("003", onlyReal = FALSE)
  almostall003 <- almostall003[almostall003 != "003"] # drop the major
  expect_equal(icd9CondenseShort(almostall003, onlyReal = FALSE), "003")

  expect_equal(icd9CondenseShort(icd9ChildrenShort("0031", onlyReal = FALSE),
                                 onlyReal = FALSE), "0031")
  # gives nothing back if a non-billable code provided, but billable requested

  expect_equal(icd9CondenseShort(c("003", othersalmonella), onlyReal = TRUE),
               "003") # onlyBillable describes input, it doesn't make any sense to describe output when condensing.
  # major is returned
  expect_equal(icd9CondenseShort(othersalmonella, onlyReal = TRUE), "003")
  expect_equal(icd9CondenseShort(othersalmonella, onlyReal = FALSE), othersalmonella)
  # now do we find a missing major if all chilren present?
  almostall003 <- icd9ChildrenShort("003", onlyReal = FALSE)
  almostall003 <- almostall003[almostall003 != "003"] # drop the major
  expect_equal(icd9CondenseShort(almostall003, onlyReal = FALSE), "003")

})

test_that("condense single major and its children", {
  expect_equal(icd9CondenseShort("003"), "003")

  skip("TODO: recode these as Explain tests")
  expect_equal(icd9ExplainShort("391"),
               "Rheumatic fever with heart involvement")
  expect_equal(icd9ExplainShort(icd9ChildrenShort("391")),
               "Rheumatic fever with heart involvement")
  expect_equal(icd9ExplainShort(icd9ChildrenShort("391", onlyReal = TRUE)),
               "Rheumatic fever with heart involvement")
})
