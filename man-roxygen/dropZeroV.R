#' @param dropZeroV single logical, if TRUE will additionally drop V0x.yz to
#'   Vx.yz In the case of 'short' form codes, this is only valid when there is
#'   no decimal part, e.g. V01 can become V1, but V0123 cannot become V123
#'   because its meaning changes.
