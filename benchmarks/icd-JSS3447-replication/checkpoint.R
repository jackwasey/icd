if (!require("checkpoint", quietly = TRUE)) {
    install.packages("checkpoint", repo = "https://cloud.r-project.org/")
    library("checkpoint", quietly = TRUE)
}

icd_checkpoint <- function(dt_str = "2018-08-23") {
  old_lib_paths <- .libPaths()
  dir.create(".checkpoint", showWarnings = FALSE)
  message("Directories")
  print(list.dirs())
  message("Current working directory:")
  print(getwd())
  checkpoint(dt_str, verbose = TRUE,
                         #project = system.file(package = "icd"),
                         #R.version = NULL,
                         checkpointLocation = getwd(), # don't pollute home directory of user
#                         use.knitr = TRUE,
#                         auto.install.knitr = TRUE,
                         scanForPackages = TRUE # must be true for docker for checkpoint to proceed
                         )
  old_lib_paths
}

