#' @param <% if (exists("icd9AnyName")) cat(icd9AnyName) else cat("icd9") %> is
#'   a character vector or factor of ICD-9 codes. If fewer than five characters
#'   is given in a code, then the digits are greedily assigned to hundreds, then
#'   tens, then units, before the decimal parts. E.g., \code{10} becomes
#'   \code{010}, not \code{0010} or \code{00010}.
