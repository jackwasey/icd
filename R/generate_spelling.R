#' Generate spelling exceptions
#'
#' Defining known spellings of names and technical terms makes the \code{aspell}
#' checks useful (and removes a NOTE in the \code{R CMD check} which at least
#' some of the CRAN people use.
#' @template save_pkg_data
#' @keywords internal
#' @noRd
.generate_spelling <- function(save_pkg_data = TRUE) {
  medical <- c(
    "anonymized",
    "arrythmia",
    "generalizable",
    "healthcare",
    "hyperemesis",
    "Infarction",
    "laterality",
    "mellitus",
    "M\u00E9ni\u00E8re's",
    "Musculoskeletal",
    "Myocardial",
    "precerebral",
    "Puerperium",
    "reproducibility"
  )
  proper_nouns <- c(
    "Anobel",
    "Aylin",
    "Burnand",
    "Charlson",
    "Christophe",
    "Deyo",
    "Deyo",
    "Druker",
    "Eddelbeuttel",
    "Eigen",
    "Elixhauser",
    "Elixhuaser",
    "Feinstein",
    "Feudtner",
    "Fong",
    "Galvez",
    "Ghali",
    "Guennebaud",
    "Hadley",
    "Hude",
    "Jarvi",
    "Kalibera",
    "Luthi",
    "Mansour",
    "Masino",
    "Odisho",
    "Petersburg",
    "ProPublica",
    "Quan",
    "Rehman",
    "RStudio",
    "Sharabiani",
    "Simpao",
    "Sundararajan",
    "Travis",
    "Ushey",
    "Vijaya",
    "Vitaly",
    "Walraven",
    "Waltham",
    "Wasey",
    "Wercker",
    "Wickham",
    "wmurphyrd",
    "Zhong"
  )
  proper_nouns <-
    c(proper_nouns, paste(proper_nouns, "'s", sep = ""))
  acronyms <- c(
    "AHRQ",
    "BISHCA",
    "BLAS",
    "CC",
    "CCs",
    "CCS",
    "CDC",
    "CIM",
    "CMS",
    "CRAN",
    "CSV",
    "DRG",
    "HCC",
    "HCCs",
    "HCUP",
    "HTN",
    "icd",
    "ICD",
    "JAMA",
    "LLVM",
    "NEDS",
    "NHDS",
    "nhds",
    "NSO",
    "PCCC",
    "PCS",
    "POA",
    "RTF",
    "SAS",
    "STL",
    "VAHHS",
    "WSU"
  )
  french <- c(
    "am\u00E9ricaine",
    "beaucoup",
    "chang\u00E9ements",
    "Comme",
    "de",
    "D\u00E9finitions",
    "des",
    "fran\u00E7aise",
    "internationale",
    "Juste",
    "l'ann\u00E9e",
    "l'\u00E9dition",
    "l'OMS",
    "le",
    "pr\u00E9sent\u00E9e",
    "statistique"
  )
  other <- c(
    "Adjustors",
    "comorbid",
    "comorbidities",
    "comorbidity",
    "hackathon",
    "MERCHANTABILITY",
    "OpenMP",
    "performant",
    "precomputed",
    "prepend",
    "prepends",
    "pseudocode",
    "Rcpp",
    "RcppEigen",
    "requestor",
    "tidyverse",
    "TODO",
    "Transuranium",
    "unclassifiable",
    "vectorization",
    "VermontDepartmentofHealth",
    "VermontHospitalDischarge",
    "VignetteEngine",
    "VignetteIndexEntry",
    "wordagain",
    "www"
  )
  words <- c(medical, proper_nouns, acronyms, french, other)
  if (save_pkg_data) {
    # per http://dirk.eddelbuettel.com/blog/2017/08/10/#008_aspell_cran_incoming
    saveRDS(words, file = ".aspell/icdwords.rds", version = 2)
    # for spelling package, write out a simple WORDLIST. This package checks
    # more stuff than the base R, but many false positives, e.g. latex figure
    # labels.
    writeLines(words, "inst/WORDLIST")
    # base R, then use aspell_package_Rd_files('.') from package root.
    utils::aspell_write_personal_dictionary_file(words,
      out = ".aspell/words.pws"
    )
  }
  invisible(words)
}
