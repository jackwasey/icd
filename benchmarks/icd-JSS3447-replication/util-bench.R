#!/usr/bin/env Rscript

have_base_fun <- function(x) {
  exists(envir = as.environment("package:base"), x)
}

sys_env_true <- function(x) {
  !(trimws(tolower(Sys.getenv(x))) %in%
    c("", "no", "false", "f", "no", "n", "0")
  )
}

is_verbose <- function() {
  sys_env_true("ICD_VERBOSE")
}

msg_verbose <- function(x, type = c("message", "warning", "print", "cat")) {
  msg_fun_name <- match.args(type)
  msg_fun <- match.fun(msg_fun_name)
  if (is_verbose()) {
    msg_fun(x)
  }
}
