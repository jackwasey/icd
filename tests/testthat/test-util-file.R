context("util-file")

test_that(".unzip_single finds the target file inside a nested zip subfolder", {
  old_opt <- options(icd.offline = FALSE)
  on.exit(options(old_opt), add = TRUE)
  src_root <- file.path(tempdir(), "icd_nested_zip_src")
  unlink(src_root, recursive = TRUE)
  sub_dir <- file.path(src_root, "a_subfolder")
  dir.create(sub_dir, recursive = TRUE)
  writeLines("hello", file.path(sub_dir, "target.txt"))
  zip_path <- tempfile(fileext = ".zip")
  old_wd <- setwd(src_root)
  on.exit(setwd(old_wd), add = TRUE)
  utils::zip(zip_path, "a_subfolder/target.txt")
  setwd(old_wd)
  save_path <- tempfile()
  ok <- icd:::.unzip_single(
    url = paste0("file://", zip_path),
    file_name = "target.txt",
    save_path = save_path
  )
  expect_true(ok)
  expect_true(file.exists(save_path))
  expect_equal(readLines(save_path), "hello")
})

test_that(".unzip_single fails clearly when the download fails", {
  old_opt <- options(icd.offline = FALSE)
  on.exit(options(old_opt), add = TRUE)
  missing_url <- paste0("file://", tempfile(fileext = ".zip"))
  expect_error(
    icd:::.unzip_single(
      url = missing_url,
      file_name = "does_not_matter.txt",
      save_path = tempfile()
    ),
    regexp = "Download failed"
  )
})
