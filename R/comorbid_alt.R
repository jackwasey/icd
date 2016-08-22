
icd10_comorbid_parent_search_str <- function(
  x,
  map,
  visit_name = NULL,
  icd_name = get_icd_name(x),
  short_code = icd_guess_short(x, icd_name = icd_name),
  short_map = icd_guess_short(map),
  return_df = FALSE, ...) {

  if (!short_code)
    x[[icd_name]] <- icd_decimal_to_short.icd10(x[[icd_name]])

  icd_codes <- x[[icd_name]]

  # for each icd code
  just_cmb <- vapply(icd_codes, FUN.VALUE = logical(length(map)), FUN = function(y) {
    # look it up in each comorbidity, but TODO: once we have a comorbidity for
    # one patient, we don't need to search within it again

    char_count <- nchar(as.character(y)):3
    vapply(names(map), FUN.VALUE = logical(1),
           FUN = function(cmb) {
             # and if not found, slice off last char of test string
             for (n in char_count) {
               if (!is.na(fmatch(substr(y, 1, n), map[[cmb]])))
                 return(TRUE)
             }
             FALSE
           })
  })

  res <- aggregate(x = t(just_cmb), by = x[visit_name], FUN = any)
  if (return_df)
    return(res)

  out <- as.matrix(res[-1])
  rownames(out) <- res[[1]]
  out
}


icd10_comorbid_parent_search_all_at_once <- function(x,
                                                     map,
                                                     visit_name = NULL,
                                                     icd_name = get_icd_name(x),
                                                     short_code = icd_guess_short(x, icd_name = icd_name),
                                                     short_map = icd_guess_short(map),
                                                     return_df = FALSE, ...) {

  if (!short_code)
    x[[icd_name]] <- icd_decimal_to_short.icd10(x[[icd_name]])

  icd_codes <- x[[icd_name]]

  # for each icd code
  just_cmb <- vapply(icd_codes, FUN.VALUE = logical(length(map)), FUN = function(y) {
    # look it up in each comorbidity, but TODO: once we have a comorbidity for
    # one patient, we don't need to search within it again


    char_count <- nchar(as.character(y)):3
    vapply(names(map), FUN.VALUE = logical(1),
           FUN = function(cmb) {
             # and if not found, slice off last char of test string
             perms_to_match <- vapply(char_count:3, substr, x = y, start = 1, FUN.VALUE = character(1))
             any(fmatch(perms_to_match, map[[cmb]], nomatch = 0L) != 0)
           })
  })

  res <- aggregate(x = t(just_cmb), by = x[visit_name], FUN = any)
  if (return_df)
    return(res)

  out <- as.matrix(res[-1])
  rownames(out) <- res[[1]]
  out
}

icd10_comorbid_parent_search_no_loop <- function(x,
                                                 map,
                                                 visit_name = NULL,
                                                 icd_name = get_icd_name(x),
                                                 short_code = icd_guess_short(x, icd_name = icd_name),
                                                 short_map = icd_guess_short(map),
                                                 return_df = FALSE, ...) {

  if (!short_code)
    x[[icd_name]] <- icd_decimal_to_short.icd10(x[[icd_name]])

  icd_codes <- x[[icd_name]]

  # TODO: unclear whether the hash map is retained by fastmatch: "The first
  # match against a table results in a hash table to be computed from the table.
  # This table is then attached as the '.match.hash' attribute of the table so
  # that it can be re-used on subsequent calls to fmatch with the same table."
  # May need to pre-compute this when building package. May still be faster even
  # if calculating hash map each time.

  # for each icd code
  just_cmb <- vapply(icd_codes, FUN.VALUE = logical(length(map)), FUN = function(y) {
    # look it up in each comorbidity, but TODO: once we have a comorbidity for
    # one patient, we don't need to search within it again

    vapply(names(map), FUN.VALUE = logical(1),
           FUN = function(cmb) {
             # instead of loop, just declare the substring length
             if (!is.na(fmatch(substr(y, 1, 10), map[[cmb]]))) return(TRUE)
             if (!is.na(fmatch(substr(y, 1, 9), map[[cmb]]))) return(TRUE)
             if (!is.na(fmatch(substr(y, 1, 8), map[[cmb]]))) return(TRUE)
             if (!is.na(fmatch(substr(y, 1, 7), map[[cmb]]))) return(TRUE)
             if (!is.na(fmatch(substr(y, 1, 6), map[[cmb]]))) return(TRUE)
             if (!is.na(fmatch(substr(y, 1, 5), map[[cmb]]))) return(TRUE)
             if (!is.na(fmatch(substr(y, 1, 4), map[[cmb]]))) return(TRUE)
             if (!is.na(fmatch(substr(y, 1, 3), map[[cmb]]))) return(TRUE)
             FALSE
           })
  })

  res <- aggregate(x = t(just_cmb), by = x[visit_name], FUN = any)
  if (return_df)
    return(res)

  out <- as.matrix(res[-1])
  rownames(out) <- res[[1]]
  out
}

icd10_comorbid_parent_search_orig <- function(x,
                                              map,
                                              visit_name = NULL,
                                              icd_name = get_icd_name(x),
                                              short_code = icd_guess_short(x, icd_name = icd_name),
                                              short_map = icd_guess_short(map),
                                              return_df = FALSE, ...) {

  if (!short_code)
    x[[icd_name]] <- icd_decimal_to_short.icd10(x[[icd_name]])

  icd_codes <- x[[icd_name]]

  # for each icd code
  just_cmb <- vapply(icd_codes, FUN.VALUE = logical(length(map)), FUN = function(y) {
    # look it up in each comorbidity, but TODO: once we have a comorbidity for
    # one patient, we don't need to search within it again


    char_count <- nchar(as.character(y)):3
    vapply(names(map), FUN.VALUE = logical(1),
           FUN = function(cmb) {
             # and if not found, slice off last char of test string
             for (n in char_count) {
               if (!is.na(fmatch(substr(y, 1, n), map[[cmb]])))
                 return(TRUE)
             }
             FALSE
           })
  })

  res <- aggregate(x = t(just_cmb), by = x[visit_name], FUN = any)
  if (return_df)
    return(res)

  out <- as.matrix(res[-1])
  rownames(out) <- res[[1]]
  out
}

icd10_comorbid_no_parent_search <- function(x,
                                            map,
                                            visit_name = NULL,
                                            icd_name = get_icd_name(x),
                                            short_code = icd_guess_short(x, icd_name = icd_name),
                                            short_map = icd_guess_short(map[[1]]),
                                            return_df = FALSE, ...) {

  # confirm class is ICD-9 so we dispatch correctly. The class may not be set if
  # the S3 method was called directly.
  if (!is.icd10(x[[icd_name]])) x[[icd_name]] <- icd10(x[[icd_name]]) # no as.icd10
  icd_comorbid_common(x, map, visit_name, icd_name,
                      short_code, short_map, return_df, ...)

}
