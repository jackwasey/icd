context("data sanity")

test_that("row numbers and factors are sequential for data frames", {
  print <- function(...) {
    base::print(..., quote = FALSE)
  }
  skip_slow()
  data_names <- c(.ls(), .ls_lazy())
  for (data_name in data_names) {
    dinfo <- paste0("get_", data_name, "()")
    if (.verbose()) print(paste0("data frame: ", data_name))
    d <- .get_anywhere(data_name)
    if (!is.data.frame(d) || grepl("map", data_name)) next
    expect_gt(nrow(d), 1)
    expect_gt(ncol(d), 1)
    expect_identical(
      row.names(d),
      as.character(
        seq_along(d[[1]])
      ),
      info = dinfo
    )
    for (col_name in names(d)) {
      col_dat <- d[[col_name]]
      .trc("now checking column: ", col_name)
      info <- paste0("get_", data_name, "()[[\"", col_name, "\"]]")
      if (col_name %nin% c("sub_sub_chapter", "age_group", "sex")) {
        if (data_name %in% c(.get_icd9cm_name(2005:2008, leaf = TRUE)) &&
          col_name == "long_desc") {
          skip("No long descriptions for older ICD-9-CM data")
        }
        expect_true(!anyNA(col_dat), info = info)
      }
      if (.verbose() > 2) {
        print(paste(info, "checking code and three_digit"))
      }
      if (col_name %in% c("code", "three_digit") &&
        !grepl(".*_pc", data_name)) {
        expect_true(inherits(col_dat, c("icd9", "icd10")), info = info)
        j <- d[[col_name]]
        expect_valid(j, whitespace_ok = FALSE, info = info)
        test_that("three-digits match the codes", {
          four_digit_majors <- nchar(as_char_no_warn(d$three_digit)) == 4
          d_three <- d[!four_digit_majors, ]
          d_four <- d[four_digit_majors, ]
          expect_equivalent(
            unclass(substr(as_char_no_warn(d_three$code), 1, 3)),
            as_char_no_warn(d_three$three_digit),
            info = dinfo
          )
          expect_equivalent(
            unclass(substr(as_char_no_warn(d_four$code), 1, 4)),
            as_char_no_warn(d_four$three_digit),
            info = dinfo
          )
        })
        if (col_name == "code") {
          expect_is(d[["code"]], "character", info = info)
        } else {
          expect_is(d[["three_digit"]], "factor", info = info)
          levs <- levels(col_dat)
          class(levs) <- sub("factor", "character", class(col_dat))
          expect_false(
            is_unsorted(levs),
            info = paste0(
              "levels(", info, "), class(levs) = ",
              paste(class(levs), collapse = ", ")
            )
          )
        }
        if (.verbose()) print(paste(info, "checking code columns are sorted"))
        if (data_name %nin% c(
          "uranium_pathology",
          "vermont_dx"
        )
        ) {
          expect_true(col_name %in% names(d))
          expect_true(!is.null(col_dat))
          if (.verbose()) {
            print(paste(
              info, "class of col_dat is: ",
              paste(class(col_dat), collapse = ", ")
            ))
          }
          expect_error(regexp = NA, us <- is_unsorted(col_dat), info = info)
          if (.verbose()) {
            print(paste(
              info, "class of is_unsorted(col_dat) is: ",
              paste(class(us), collapse = ", ")
            ))
          }
          expect_false(is_unsorted(col_dat), info = info)
        }
        expect_classes_ordered(col_dat)
        if (inherits(col_dat, icd9_classes)) {
          expect_is(col_dat, "icd9")
        }
        if (inherits(col_dat, icd10_classes)) {
          expect_is(col_dat, "icd10")
        }
        # end code or three digit only block
      } else {
        # columns not named code or three_digit should not inherit an ICD code type?
        expect_false(inherits(col_dat, icd_data_classes))
      }
    } # for each col_name
  } # for each data.frame
})
