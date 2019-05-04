.icd9cm_sources <- local({
  cms_base <-
    "https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/"
  cdc_base <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/"
  data.frame(
    version = as.character(seq(32, 23)),
    f_year = c(as.character(seq(2014, 2005))),
    start_date = c(
      "2014-10-01", "2013-10-01", "2012-10-01", "2011-10-01",
      "2010-10-01", "2009-10-01", "2008-10-01", "2007-10-01",
      "2006-10-01", "2005-10-01"
    ),
    long_filename = c(
      "CMS32_DESC_LONG_DX.txt",
      "CMS31_DESC_LONG_DX.txt",
      "CMS30_DESC_LONG_DX 080612.txt",
      "CMS29_DESC_LONG_DX.101111.txt",
      "CMS28_DESC_LONG_DX.txt",
      NA, # see other filename
      NA, # no long descriptions available for these years
      NA,
      NA,
      NA
    ),
    short_filename = c(
      "CMS32_DESC_SHORT_DX.txt",
      "CMS31_DESC_SHORT_DX.txt",
      "CMS30_DESC_SHORT_DX.txt",
      "CMS29_DESC_SHORT_DX.txt",
      "CMS28_DESC_SHORT_DX.txt",
      NA,
      "V26 I-9 Diagnosis.txt", # nolint
      "I9diagnosesV25.txt",
      "I9diagnosis.txt",
      "I9DX_DESC.txt"
    ),
    other_filename = c(
      NA, NA, NA, NA, NA,
      "V27LONG_SHORT_DX_110909.csv",
      # "V27LONG_SHORT_DX_110909u021012.csv" is 'updated' but
      # hasn't got correctly formatted <3digit codes.
      NA, NA, NA, NA
    ),
    long_encoding = c(
      "latin1", "latin1", "latin1",
      "latin1", "latin1", "latin1",
      NA, NA, NA, NA
    ),
    short_encoding = rep_len("ASCII", 10),
    url = c(
      paste0(cms_base, "ICD-9-CM-v32-master-descriptions.zip"),
      paste0(cms_base, "cmsv31-master-descriptions.zip"),
      paste0(cms_base, "cmsv30_master_descriptions.zip"),
      paste0(cms_base, "cmsv29_master_descriptions.zip"),
      paste0(cms_base, "cmsv28_master_descriptions.zip"),
      paste0(cms_base, "FY2010Diagnosis-ProcedureCodesFullTitles.zip"),
      # but this one is in a different format! only contains short descs:
      # paste0(cms_base, "v27_icd9.zip",
      paste0(cms_base, "v26_icd9.zip"),
      paste0(cms_base, "v25_icd9.zip"),
      paste0(cms_base, "v24_icd9.zip"),
      paste0(cms_base, "v23_icd9.zip")
    ),
    # FY11,12,13,14 are the same?
    rtf_url = c(
      rep_len(paste0(cdc_base, "ICD9-CM/2011/Dtab12.zip"), 4),
      paste0(cdc_base, "ICD9-CM/2010/DTAB11.zip"),
      paste0(cdc_base, "ICD9-CM/2009/Dtab10.zip"),
      paste0(cdc_base, "ICD9-CM/2008/Dtab09.zip"),
      paste0(cdc_base, "ICD9-CM/2007/Dtab08.zip"),
      paste0(cdc_base, "ICD9-CM/2006/Dtab07.zip"),
      paste0(cdc_base, "ICD9-CM/2005/Dtab06.zip")
    ),
    # there are more RTF files, but not with corresponding CMS, back to 1990s
    rtf_filename = c(
      rep_len("Dtab12.rtf", 4),
      "DTAB11.RTF", "Dtab10.RTF", "Dtab09.RTF", "Dtab08.RTF",
      "Dtab07.RTF", "Dtab06.rtf"
    ),
    stringsAsFactors = FALSE
  )
})

.icd10cm_sources <- local({
  list(
    "2019" = list(
      base_url = "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/",
      dx_zip = "2019-ICD-10-CM-Code-Descriptions.zip",
      dx_xml_zip = "2019-ICD-10-CM-Tables-and-Index.zip",
      dx_xml = "icd10cm_tabular_2019.xml",
      dx_leaf = "icd10cm_codes_2019.txt",
      dx_hier = "icd10cm_order_2019.txt",
      pcs_zip = "2019-ICD-10-PCS-Order-File.zip",
      pcs_xml_zip = "2019-ICD-10-PCS-Tables-And-Index.zip",
      pcs_flat = "icd10pcs_order_2019.txt"
    ),
    "2018" = list(
      base_url = "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/",
      dx_zip = "2018-ICD-10-Code-Descriptions.zip",
      dx_xml_zip = "2018-ICD-10-Table-And-Index.zip",
      dx_xml = "Tabular.xml",
      dx_leaf = "icd10cm_codes_2018.txt",
      dx_hier = "icd10cm_order_2018.txt",
      pcs_zip = "2018-ICD-10-PCS-Order-File.zip",
      pcs_xml_zip = "2018-ICD-10-PCS-Tables-And-Index.zip",
      pcs_flat = "icd10pcs_order_2018.txt"
    ),
    "2017" = list(
      base_url = "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/",
      dx_zip = "2017-ICD10-Code-Descriptions.zip",
      pcs_zip = "2017-PCS-Long-Abbrev-Titles.zip",
      pcs_xml_zip = "2017-PCS-Code-Tables.zip",
      dx_xml = "Tabular.xml",
      dx_xml_zip = "2017-ICD10-Code-Tables-Index.zip",
      dx_leaf = "icd10cm_codes_2017.txt",
      dx_hier = "icd10cm_order_2017.txt",
      pcs_flat = "icd10pcs_order_2017.txt"
    ),
    "2016" = list(
      base_url = "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/",
      dx_zip = "2016-Code-Descriptions-in-Tabular-Order.zip",
      dx_xml_zip = "2016-CM-Code-Tables-and-Index.zip",
      dx_xml = "Tabular.xml",
      dx_leaf = "icd10cm_codes_2016.txt",
      dx_hier = "icd10cm_order_2016.txt",
      pcs_zip = "2016-PCS-Long-Abbrev-Titles.zip",
      pcs_xml_zip = "2016-PCS-Code-Tables.zip",
      pcs_flat = "icd10pcs_order_2016.txt"
    ),
    "2015" = list(
      base_url = "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/",
      dx_zip = "2015-code-descriptions.zip",
      dx_xml_zip = "2015-tables-index.zip",
      dx_xml = "Tabular.xml",
      dx_leaf = "icd10cm_codes_2015.txt",
      dx_hier = "icd10cm_order_2015.txt",
      pcs_zip = "2015-PCS-long-and-abbreviated-titles.zip",
      pcs_xml_zip = "2015-Code_Tables-and-Index.zip",
      pcs_flat = "icd10pcs_order_2015.txt"
    ),
    "2014" = list(
      base_url = "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/",
      dx_zip = "2014-ICD10-Code-Descriptions.zip",
      dx_xml_zip = "2014-ICD10-Code-Tables-and-Index.zip",
      dx_xml = "Tabular.xml",
      dx_leaf = "icd10cm_codes_2014.txt",
      dx_hier = "icd10cm_order_2014.txt",
      pcs_zip = "2014-PCS-long-and-abbreviated-titles.zip",
      pcs_xml_zip = "2014-Code-Tables-and-Index.zip",
      pcs_flat = "icd10pcs_order_2014.txt"
    )
  )
})

.url_ok <- function(url) {
  httr::HEAD(url)$status_code < 400
}

.url_warn_or_stop <- function(url, warn = FALSE) {
  if (!.url_ok(url)) {
    if (warn) {
      warning("NA: ", url, call. = FALSE)
    } else {
      stop("NA: ", url, call. = FALSE)
    }
  }
  else {
    message("OK: ", url)
  }
}

.check_icd9cm_urls <- function(warn = FALSE) {
  oldwarn <- options("warn" = 1)
  on.exit(options(oldwarn))
  urls <- c(.icd9cm_sources$url, .icd9cm_sources$rtf_url)
  for (url in urls) {
    .url_warn_or_stop(url, warn)
  }
  message("now regenerate sysdata with\ngenerate_sysdata()")
}

.check_icd10cm_urls <- function(warn = FALSE) {
  oldwarn <- options("warn" = 1)
  on.exit(options(oldwarn))
  lapply(.icd10cm_sources, function(year) {
    zips <- grep("zip$", names(year))
    urls <- paste0(year$base_url, unlist(unname(year))[zips])
    for (url in urls) {
      .url_warn_or_stop(url, warn)
    }
  })
}
