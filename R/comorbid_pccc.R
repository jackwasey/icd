globalVariables(c(
  "code_name", "icd10_map_pccc_dx", "icd9_map_pccc_dx",
  "icd9_map_pccc_pcs", "icd10_map_pccc_pcs"
))

#' Calculate pediatric complex chronic conditions (PCCC) comorbidities
#'
#' Unlike with ICD-9 and ICD-10 diagnostic codes, 'icd' doesn't
#' currently have a method for guessing which fields are procedure
#' codes, so \code{icd_name} must be specified for the \code{_pcs}
#' functions.
#' @inheritParams comorbid
#' @examples
#' # not pediatric data, but let's look for this example
#' head(icd9_comorbid_pccc_dx(icd.data::vermont_dx[1:1000, ]))
#' @export
comorbid_pccc_dx <- function(x,
                             visit_name = get_visit_name(x),
                             icd_name = get_icd_name(x),
                             short_code = guess_short(x, icd_name = icd_name),
                             return_df = FALSE,
                             return_binary = FALSE,
                             ...) {
  switch_ver_cmb(
    x = x, funs = list(
      icd9 = icd9_comorbid_pccc_dx,
      icd10 = icd10_comorbid_pccc_dx
    ),
    visit_name = visit_name, icd_name = icd_name,
    short_code = short_code, return_df = return_df,
    return_binary = return_binary, ...
  )
}

#' @describeIn comorbid_pccc_dx Calculate the PCCC comorbidities based
#'   on procedure codes,
#' @examples
#' # Six random codes from each PCCC procedure code map. 'icd' will use
#' # an heuristic to guess whether ICD-9 or ICD-10:
#' pts <- data.frame(
#'   encounters = c(10, 11, 12),
#'   icd9_pcs = c("0152", "304", "0050"),
#'   icd10_pcs = c("0B110Z4", "02YA0Z2", "031209D")
#' )
#' comorbid_pccc_pcs(pts, icd_name = "icd9_pcs", return_binary = TRUE)
#' comorbid_pccc_pcs(pts, icd_name = "icd10_pcs", return_binary = TRUE)
#'
#' # All ICD-9 procedure codes are numeric, some ICD-10 procedure codes
#' # are numeric, so best to call functions directly:
#' pts <- data.frame(encounters = c(100), icd10_pcs = c("0016070"))
#' icd10_comorbid_pccc_pcs(pts, icd_name = "icd10_pcs")
#' @family comorbidity computations
#' @family comorbidities
#' @export
comorbid_pccc_pcs <- function(x,
                              visit_name = get_visit_name(x),
                              icd_name,
                              return_df = FALSE,
                              return_binary = FALSE,
                              ...) {
  stopifnot(visit_name %in% names(x), icd_name %in% names(x))
  n <- min(10L, length(x[[icd_name]]))
  test_some <- as.character(x[seq_len(n), icd_name])
  nines_tens <- grepl(".*[A-Za-z].*", test_some, ignore.case = TRUE)
  is_icd9 <- TRUE
  threshold <- 0.7
  if (sum(nines_tens) / n > threshold) {
    is_icd9 <- FALSE
  } else if (sum(nines_tens) / n > (1 - threshold)) {
    warning(
      "many invalid ICD-9 procedure codes, but not enough to ",
      "determine that all the codes were ICD-10 codes. Please",
      "check the input data. Assuming ICD-9."
    )
  }
  if (is_icd9) {
    icd9_comorbid_pccc_pcs(x,
      visit_name = visit_name,
      icd_name = icd_name,
      return_df = return_df,
      return_binary = return_binary,
      ...
    )
  } else {
    icd10_comorbid_pccc_pcs(x,
      visit_name = visit_name,
      icd_name = icd_name,
      return_df = return_df,
      return_binary = return_binary,
      ...
    )
  }
}

#' @describeIn comorbid_pccc_dx Calculate PCCC comorbidities from ICD-9
#'   diagnosis codes
#' @export
icd9_comorbid_pccc_dx <-
  function(x,
             visit_name = NULL,
             icd_name = NULL,
             short_code = guess_short(x, icd_name = icd_name),
             return_df = FALSE,
             return_binary = FALSE,
             ...) {
    icd9_comorbid(
      x = x,
      map = icd9_map_pccc_dx,
      visit_name = visit_name,
      icd_name = icd_name,
      short_code = short_code,
      short_map = TRUE,
      return_df = return_df,
      return_binary = return_binary,
      ...
    )
  }

#' @describeIn comorbid_pccc_dx Calculate PCCC comorbidities from ICD-10
#'   diagnosis codes
#' @export
icd10_comorbid_pccc_dx <- function(x,
                                   visit_name = NULL,
                                   icd_name = NULL,
                                   short_code = guess_short(x, icd_name = icd_name),
                                   return_df = FALSE, return_binary = FALSE, ...) {
  icd10_comorbid(
    x = x,
    map = icd10_map_pccc_dx,
    visit_name = visit_name,
    icd_name = icd_name,
    short_code = short_code,
    short_map = TRUE,
    return_df = return_df,
    return_binary = return_binary,
    ...
  )
}

#' @describeIn comorbid_pccc_dx Calculate PCCC comorbidities from ICD-9
#'   procedure codes
#' @export
icd9_comorbid_pccc_pcs <- function(x,
                                   visit_name = get_visit_name(x),
                                   icd_name = get_icd_pc_name(x),
                                   return_df = FALSE,
                                   return_binary = FALSE,
                                   ...) {
  if (is.null(icd_name)) {
    stop(
      "No ICD procedure codes found. ",
      "You may set the class of the columns in the ",
      "data frame (e.g., to \"icd10cm_pc\", if not ",
      "auto-detected."
    )
  }
  categorize_simple(
    x = x,
    map = icd9_map_pccc_pcs,
    id_name = visit_name,
    code_name = icd_name,
    return_df = return_df,
    return_binary = return_binary,
    ...
  )
}

#' @describeIn comorbid_pccc_dx Calculate PCCC comorbidities from ICD-10
#'   procedure codes
#' @export
icd10_comorbid_pccc_pcs <- function(x,
                                    visit_name = get_visit_name(x),
                                    icd_name,
                                    return_df = FALSE,
                                    return_binary = FALSE,
                                    ...) {
  categorize_simple(
    x = x,
    map = icd10_map_pccc_pcs,
    id_name = visit_name, code_name = icd_name,
    return_df = return_df,
    return_binary = return_binary,
    ...
  )
}
