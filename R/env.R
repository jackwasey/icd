# Copyright (C) 2014 - 2016  Jack O. Wasey
#
# This file is part of icd.
#
# icd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd. If not, see <http:#www.gnu.org/licenses/>.

#' create environment from vector
#'
#' create an environment by inserting the value \code{val} with names taken from
#' \code{x}
#' @keywords internal
vec_to_env <- function(x, val = TRUE, env = new.env(hash = TRUE, parent = baseenv())) {
  lapply(x, function(y) { env[[y]] <- val; return()} )
  return(invisible(env))
}

vec_to_env_count <- function(x, env = new.env(hash = TRUE, parent = baseenv())) {
  for (i in 1L:length(x)) {
    env[[x[i]]] <- i
  }
  invisible(env)
}

#' return a new environment with names and values swapped
#' @param env environment with values being sequence numbers used to fill
#'   returned vector
#' @keywords internal
env_to_vec_flip <- function(env) {
  assert_environment(env)
  out <- character(length(env))
  lapply(ls(env), function(y) out[env[[y]]] <<- y)
  out
}

env_to_lookup_pair <- function(env) {
  env_vals <- mget(envir = env, ls(env))
  stopifnot(is.integer(env_vals[[1L]]))
  stopifnot(anyDuplicated(as.integer(env_vals)))
  list(env, env_to_vec_flip(env))
}

vec_to_lookup_pair <- function(x, env = new.env(hash = TRUE, parent = baseenv())) {
  for (i in 1L:length(x)) {
    env[[x[i]]] <- i
  }
  invisible(list(env = env, vec = x))
}
