#' @param icd_name The column in the data frame which contains the ICD codes.
#'   This is a character vector of length one. If it is \code{NULL}, \code{icd9}
#'   will attempt to guess the column name, looking for progressively less
#'   likely possibilities until it matche a single column. Failing this, it will
#'   take the first column in the data frame. Specifying the column using this
#'   argument avoids the guesswork.
#' @param icd9Field Deprecated. Use \code{icd_name}
