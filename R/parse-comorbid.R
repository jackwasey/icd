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

ahrq_htn <- c("HTNCX", "HTNPREG", "OHTNPREG", "HTNWOCHF", "HTNWCHF", "HRENWORF",
              "HRENWRF", "HHRWOHRF", "HHRWCHF", "HHRWRF", "HHRWHRF")
ahrq_chf <- c("CHF", "HTNWCHF", "HHRWCHF", "HHRWHRF")
ahrq_renal <- c("RENLFAIL", "HRENWRF", "HHRWRF", "HHRWHRF")
ahrq_unused <- c("HTNPREG", "OHTNPREG", "HTNWOCHF", "HTNWCHF", "HRENWORF", "HRENWRF",
                 "HHRWOHRF", "HHRWCHF", "HHRWRF", "HHRWHRF")

ahrq_order <- c("CHF", "VALVE", "PULMCIRC", "PERIVASC", "HTN", "HTNCX", "PARA",
                "NEURO", "CHRNLUNG", "DM", "DMCX", "HYPOTHY", "RENLFAIL", "LIVER",
                "ULCER", "AIDS", "LYMPH", "METS", "TUMOR", "ARTH", "COAG", "OBESE",
                "WGHTLOSS", "LYTES", "BLDLOSS", "ANEMDEF", "ALCOHOL", "DRUG",
                "PSYCH", "DEPRESS")

ahrq_order_all <- c("CHF", "VALVE", "PULMCIRC", "PERIVASC", "HTN", "HTNCX", "HTNPREG",
                    "HTNWOCHF", "HTNWCHF", "HRENWORF", "HRENWRF", "HHRWOHRF", "HHRWCHF",
                    "HHRWRF", "HHRWHRF", "OHTNPREG", "PARA", "NEURO", "CHRNLUNG",
                    "DM", "DMCX", "HYPOTHY", "RENLFAIL", "LIVER", "ULCER", "AIDS",
                    "LYMPH", "METS", "TUMOR", "ARTH", "COAG", "OBESE", "WGHTLOSS",
                    "LYTES", "BLDLOSS", "ANEMDEF", "ALCOHOL", "DRUG", "PSYCH", "DEPRESS")

#' get the SAS code from AHRQ
#'
#' Get the SAS code from AHRQ and save in data-raw if not already there.
#' @keywords internal
icd9_fetch_ahrq_sas <- function(offline) {
  download_to_data_raw(
    url = "http://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comformat2012-2013.txt",
    offline = offline)
}

icd10_fetch_ahrq_sas <- function(offline) {
  download_to_data_raw(
    url = "http://www.hcup-us.ahrq.gov/toolssoftware/comorbidityicd10/comformat_icd10cm_2016.txt",
    offline = offline)
}

#' parse AHRQ SAS code to get mapping
#'
#' Raw data taken directly from the AHRQ web site and parsed. It is then saved
#' in the development tree data directory, so this is an internal function, used
#' in generating the package itself.
#' @template parse-template
#' @keywords internal
icd9_parse_ahrq_sas <- function(save_data = FALSE) {
  assert_flag(save_data)

  # readLines make assumptions or guess about encoding, consider using
  # Hadleyverse for this in future
  ahrq_info <- icd9_fetch_ahrq_sas(offline = TRUE)

  ahrq_sas_lines <- readLines(ahrq_info$file_path)
  icd9_map_ahrq_working <- sas_format_extract_rcomfmt(ahrq_sas_lines)
  icd9_map_ahrq <- list()

  for (cmb in names(icd9_map_ahrq_working)) {
    message("parsing AHRQ SAS codes for '", cmb, "'")
    some_pairs <- strsplit(x = icd9_map_ahrq_working[[cmb]], split = "-")

    # non-range values (and their children) just go on list
    unpaired_items <- sapply(some_pairs, length) == 1
    out <- c()
    if (any(unpaired_items))
      out <- icd_children.icd9(unlist(some_pairs[unpaired_items]), defined = FALSE, short_code = TRUE)

    the_pairs <- some_pairs[lapply(some_pairs, length) == 2]
    out <- c(out, lapply(the_pairs, function(x) sas_expand_range(x[1], x[2])))
    # update icd9_map_ahrq with full range of icd9 codes:
    out %>% unlist %>% unique %>% as.icd9 %>% as.icd_short_diag -> icd9_map_ahrq[[cmb]]
  }

  # drop this superfluous finale which allocates any other ICD-9 code to the
  # "Other" group
  icd9_map_ahrq[[" "]] <- NULL
  icd9_map_ahrq[ahrq_htn] %>% unlist %>% unname %>% as.icd9 %>% as.icd_short_diag -> icd9_map_ahrq[["HTNCX"]]
  icd9_map_ahrq[ahrq_chf] %>% unlist %>% unname %>% as.icd9 %>% as.icd_short_diag -> icd9_map_ahrq[["CHF"]]
  icd9_map_ahrq[ahrq_renal] %>% unlist %>% unname %>% as.icd9 %>% as.icd_short_diag -> icd9_map_ahrq[["RENLFAIL"]]

  icd9_map_ahrq[ahrq_unused] <- NULL

  # officially, AHRQ HTN with complications means that HTN on its own should be
  # unset. however, this is not feasible here, since we just package up the data
  # into a list, and it can be used however the user wishes. It would not be
  # hard to write an AHRQ specific function to do this if needed, but it makes
  # more sense to me

  #   condense to parents, for each parent, if children are all in the list, add
  #   the parent
  for (cmb in names(icd9_map_ahrq)) {
    message("working on ranges for: ", cmb)
    parents <- icd_condense.icd9(icd9_map_ahrq[[cmb]], defined = FALSE,
                                 short_code = TRUE)
    for (p in parents) {
      kids <- icd_children.icd9(p, defined = FALSE, short_code = TRUE)
      kids <- kids[-which(kids == p)] # don't include parent in test
      if (all(kids %in% icd9_map_ahrq[[cmb]]))
        icd9_map_ahrq[[cmb]] <- c(icd9_map_ahrq[[cmb]], p) %>%
        unique %>%
        icd_sort.icd9(short_code = TRUE) %>%
        as.icd9 %>% as.icd_short_diag
    }
  }

  names(icd9_map_ahrq) <- icd::icd_names_ahrq_htn_abbrev
  icd9_map_ahrq %<>% icd_comorbidity_map

  if (save_data)
    save_in_data_dir("icd9_map_ahrq")

  invisible(icd9_map_ahrq)
}

# This is in some ways simpler than that ICD-9 equivalent because I make no
# attempt to find all the child codes.
icd10_parse_ahrq_sas <- function(save_data = FALSE) {
  assert_flag(save_data)

  ahrq_info <- icd10_fetch_ahrq_sas(offline = TRUE)

  ahrq_sas_lines <- readLines(ahrq_info$file_path)
  icd10_map_ahrq <- sas_format_extract_rcomfmt(ahrq_sas_lines)

  icd10_map_ahrq[ahrq_htn] %>% unlist %>% unname -> icd10_map_ahrq[["HTNCX"]]
  icd10_map_ahrq[ahrq_chf] %>% unlist %>% unname -> icd10_map_ahrq[["CHF"]]
  icd10_map_ahrq[ahrq_renal] %>% unlist %>% unname -> icd10_map_ahrq[["RENLFAIL"]]

  icd10_map_ahrq[ahrq_unused] <- NULL

  # put in the same order as the ICD-9 listings (and the publications)
  icd10_map_ahrq <- icd10_map_ahrq[match(ahrq_order, names(icd10_map_ahrq))]

  names(icd10_map_ahrq) <- icd::icd_names_ahrq_htn_abbrev
  icd10_map_ahrq <- lapply(icd10_map_ahrq, as.icd_short_diag)
  icd10_map_ahrq <- lapply(icd10_map_ahrq, as.icd10)
  icd10_map_ahrq %<>% icd_comorbidity_map

  if (save_data)
    save_in_data_dir("icd10_map_ahrq")

  invisible(icd10_map_ahrq)
}

#' @keywords internal
icd9_fetch_quan_deyo_sas <- function(...) {
  download_to_data_raw(
    url =
      "http://mchp-appserv.cpe.umanitoba.ca/concept/ICD9_E_Charlson.sas.txt",
    file_name = "ICD9_E_Charlson.sas", ...)
}

#' parse original SAS code defining Quan's update of Deyo comorbidities.
#'
#' As with \code{parseAhrqSas}, this function reads SAS code, and
#'   in, a very limited way, extracts definitions. In this case the code uses
#'   LET statements, with strings or lists of strings. This saves and invisibly
#'   returns a list with names corresponding to the comorbidities and values as
#'   a vector of 'short' form (i.e. non-decimal) ICD9 codes. Unlike
#'   \code{icd9_parse_ahrq_sas}, there are no ranges defined, so this
#'   interpretation is simpler.
#'
#'   With thanks to Dr. Quan, I have permission to distribute his SAS code.
#'   Previously, the SAS code would be downloaded from the University of
#'   Manitoba at
#'   \url{http://mchp-appserv.cpe.umanitoba.ca/concept/ICD9_E_Charlson.sas.txt}.
#'   There are structural differences between this version and the version
#'   directly from Dr. Quan, however, the parsing results in identical data.
#' @template parse-template
#' @template offline
#' @keywords internal
icd9_parse_quan_deyo_sas <- function(save_data = FALSE) {
  assert_flag(save_data)

  # download the file and/or just get the path or file name, fails if missing
  # by default
  f_info <- icd9_fetch_quan_deyo_sas(offline = TRUE)

  quan_sas_lines <- readLines(f_info$file_path, warn = FALSE)
  let_statements <- sas_extract_let_strings(quan_sas_lines)
  icd9_map_quan_deyo <- let_statements[grepl("DC[[:digit:]]+", names(let_statements))]

  # use validation: takes time, but these are run-once per package creation (and
  # test) tasks.
  icd9_map_quan_deyo <- lapply(icd9_map_quan_deyo, icd_children.icd9,
                               short_code = TRUE, defined = FALSE)

  # do use icd:: to refer to a lazy-loaded dataset which is obscurely within
  # the package, but not in its namespace, or something...
  names(icd9_map_quan_deyo) <- icd::icd_names_charlson_abbrev
  icd9_map_quan_deyo %<>% as.icd_short_diag %>% icd9 %>% icd_comorbidity_map

  if (save_data)
    save_in_data_dir(icd9_map_quan_deyo)
  invisible(icd9_map_quan_deyo)
}
