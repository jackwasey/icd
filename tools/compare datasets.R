# compare data-sets when data built on different platforms:
library(icd)
library(crayon)
if (!exists("e", mode = "environment") ||
    length(ls(envir = e)) == 0) {
  d2_dir <- "/tmp/d3"
  e <- new.env()
  nms <- lapply(list.files(d2_dir, full.names = TRUE), load, envir = e)
  nms <- unlist(nms)
}
for (n in nms) {
  message(blue("Working on "), yellow(n))
  xo <- get(x = n, envir = as.environment("package:icd"))
  xe <- get(x = n, envir = e)
  if (identical(xo, xe)) {
    message(green("Identical"))
    next
  }
  print(testthat::compare(xo, xe))
}
