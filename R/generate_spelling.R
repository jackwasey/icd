#' Generate spelling exceptions
#'
#' This is a bit painful, but defining known spellings of names and technical
#' terms makes the \code{aspell} checks useful (and removes a NOTE in the
#' \code{R CMD check} which at least some of the CRAN people use.
#' @template save_data
#' @keywords internal
generate_spelling <- function(save_data = TRUE) {
  medical <- c(
    "arrythmia", "Musculoskeletal", "Myocardial",
    "Puerperium", "laterality", "Infarction", "healthcare",
    "mellitus", "Meniere's", "anonymized", "reproducibility",
    "generalizable"
  )
  proper_nouns <- c(
    "Charlson", "Deyo", "Elixhauser", "Mansour", "Quan",
    "Sharabiani", "Travis", "Walraven", "Wasey", "Wickham",
    "wmurphyrd", "Hadley", "Eigen", "Eddelbeuttel", "Druker",
    "Deyo", "Simpao", "Vitaly", "Ushey", "Sundararajan",
    "Rehman", "Elixhuaser", "Anobel", "Waltham", "Zhong",
    "Vijaya", "Odisho", "Petersburg", "Masino", "Luthi",
    "Jarvi", "Hude", "Guennebaud", "Galvez", "Fong", "Ghali",
    "Aylin", "Feudtner", "Feinstein", "Burnand", "Christophe"
  )
  proper_nouns <- c(proper_nouns, paste(proper_nouns, "'s", sep = ""))
  acronyms <- c(
    "AHRQ", "BISHCA", "CC", "CCs", "CCS", "CRAN", "CDC", "CMS",
    "CSV", "DRG", "POA", "HTN", "ICD", "HCC", "HCCs", "icd", "JAMA",
    "RTF", "SAS", "VAHHS", "NSO", "WSU", "PCCC", "HCUP", "BLAS",
    "PCS"
  )
  other <- c(
    "comorbidity", "comorbidities", "comorbid",
    "unclassifiable", "requestor", "Transuranium", "TODO",
    "MERCHANTABILITY", "prepend", "prepends", "precomputed",
    "Adjustors", "wordagain", "VignetteIndexEntry", "VignetteEngine",
    "VermontHospitalDischarge", "VermontDepartmentofHealth", "www",
    "Rcpp", "RcppEigen"
  )
  # "requestor" in Vermont data licence
  words <- c(medical, proper_nouns, acronyms, other)
  if (save_data) {
    # per http://dirk.eddelbuettel.com/blog/2017/08/10/#008_aspell_cran_incoming
    saveRDS(words, file = ".aspell/icdwords.rds")
    # for spelling package
    writeLines(words, "inst/WORDLIST")
    # obsolete way?
    utils::aspell_write_personal_dictionary_file(words,
      out = ".aspell/words.pws"
    )
  }
  invisible(words)
}
