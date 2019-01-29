globalVariables(c("icd10_map_cc", "icd9_map_cc", "icd_map_cc_hcc"))

#' Get Hierarchical Condition Codes (HCC)
#'
#' Applying CMS Hierarchical Condition Categories \code{comorbid_hcc} works
#' differently from the rest of the comorbidity assignment functions. This is
#' because CMS publishes a detailed ICD to Condition Category mapping including
#' all child ICD codes. While these mappings were the same for 2007-2012, after
#' 2013 there are annual versions, so date must be taken into consideration.
#' Also, there is a many:many linkage between ICD and Condition Categories (CC).
#' Once CCs are assigned, a series of hierarchy rules (which can also change
#' annually) are applied to create the HCCs.
#' @param x data frame with columns for patient/visit ID, ICD code and date
#' @param date_name the name of the column representing the date of each record.
#'   Needed because each year there is a different ICD9/10 to CC mapping).
#'   Default value is 'date'.
#' @template visit_name
#' @template icd_name
#' @family comorbidity computations
#' @family comorbidities
#' @export
comorbid_hcc <- function(x, date_name = "date",
                         visit_name = get_visit_name(x),
                         icd_name = get_icd_name(x))
  switch_ver_cmb(x, list(icd9 = icd9_comorbid_hcc,
                         icd10 = icd10_comorbid_hcc),
                 date_name = date_name, visit_name = visit_name,
                 icd_name = icd_name)

#' @describeIn comorbid_hcc Get HCCs from a data frame of ICD-9 codes
#' @export
icd9_comorbid_hcc <- function(x,
                              date_name = "date",
                              visit_name = NULL,
                              icd_name = NULL)
  comorbid_hcc_worker(x,
                      map = icd9_map_cc,
                      date_name = date_name,
                      visit_name = visit_name,
                      icd_name = icd_name)

#' @describeIn comorbid_hcc Get HCCs from a data frame of ICD-10 codes
#' @export
icd10_comorbid_hcc <- function(x,
                               date_name = "date",
                               visit_name = NULL,
                               icd_name = NULL) {
  comorbid_hcc_worker(x,
                      map = icd10_map_cc,
                      date_name = date_name,
                      visit_name = visit_name,
                      icd_name = icd_name)
}
#' apply HCC rules to either ICD-9 or ICD-10 codes
#'
#' @keywords internal manip
comorbid_hcc_worker <- function(x,
                                map,
                                date_name,
                                visit_name,
                                icd_name) {
  stopifnot(is.data.frame(x))
  stopifnot(ncol(x) >= 3L)
  stopifnot(!anyDuplicated(names(x)))
  stopifnot(is.null(visit_name) ||
              (is.character(visit_name) && length(visit_name) == 1L))
  stopifnot(is.null(icd_name) || is.character(icd_name))
  visit_name <- get_visit_name(x, visit_name)
  icd_name <- get_icd_name(x, icd_name)
  assert_string(date_name)
  assert_string(visit_name)
  assert_string(icd_name)
  x$year <- as.numeric(format(x[[date_name]], "%Y"))
  # merge CCs to patient data based on ICD and year drop ICD info
  x <- merge(x, map, all.x = TRUE,
             by.x = c(icd_name, "year"),
             by.y = c("icd_code", "year"))
  # Drop missing CC and convert to numeric
  # Not all ICDs resolve to a CC by definition
  x <- x[!is.na(x$cc), ]
  x$cc <- as.numeric(x$cc)
  # keep id, date, and cc columns only, reorder
  x <- x[, c(visit_name, date_name, "year", "cc")]
  # Keep only unique records
  # Multiple ICDs for a patient can resolve to same CC
  x <- unique(x)
  # Duplicate the ifcc column needed for future matching
  hierarchy <- icd_map_cc_hcc
  hierarchy$cc <- hierarchy$ifcc
  # Merge hierarchy rules with patient data
  x <- merge(x, hierarchy, all.x = TRUE)
  # Create a list of dataframes that contain the CCs that will be zeroed out
  todrop <- list()
  for (i in 1:6) todrop[[i]] <- x[!is.na(x$ifcc), c(3, 4, 5 + i)]
  # Rename all dataframes in list to same column names
  # rbind into a single dataframe
  todrop <- lapply(seq_along(todrop), function(x) {
    names(todrop[[x]]) <- c(visit_name, date_name, "cc")
    todrop[[x]]
  })
  todrop <- do.call(rbind, todrop)
  # Remove all NAs from CC field
  todrop <- todrop[!is.na(todrop$cc), ]
  if (nrow(todrop) > 0) {
    # Set flag for all of the CCs to be dropped
    todrop$todrop <- TRUE
    # Merge drop flags with patient data
    x <- merge(x, todrop, all.x = TRUE)
    # Drop flagged patients and keep columns of interest
    x <- x[is.na(x$todrop), ]
  }
  x <- x[, c(visit_name, date_name, "cc")]
  names(x) <- c(visit_name, date_name, "hcc")
  x
}
