if (!require("checkpoint", quietly = TRUE)) {
  install.packages("checkpoint", repo = "https://cloud.r-project.org/")
  library("checkpoint", quietly = TRUE)
}

icd_checkpoint <- function(dt_str = "2018-08-23") {
  old_lib_paths <- .libPaths()
  dir.create(".checkpoint", showWarnings = FALSE)
  checkpoint(dt_str, verbose = TRUE,
             R.version = NULL,
             # don't pollute home directory of user with default
             checkpointLocation = getwd(),
             # must be true for docker for checkpoint to proceed
             scanForPackages = TRUE
  )
  old_lib_paths
}
