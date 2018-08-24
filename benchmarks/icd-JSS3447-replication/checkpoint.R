icd_checkpoint <- function(dt_str = "2018-08-23") {
  if (!requireNamespace("checkpoint")) install.packages("checkpoint")
  old_lib_paths <- .libPaths()
  dir.create(".checkpoint", showWarnings = FALSE)
  checkpoint::checkpoint(dt_str,
                         #project = system.file(package = "icd"),
                         R.version = NULL,
                         checkpointLocation = getwd(),
                         use.knitr = TRUE,
                         auto.install.knitr = TRUE,
                         scanForPackages = TRUE
                         )
  old_lib_paths
}

