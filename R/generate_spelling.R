#' Generate spelling exceptions
#'
#' This is a bit painful, but defining known spellings of names and technical
#' terms makes the aspell checks useful (and removes a NOTE in the R CMD check
#' which at least some of the CRAN people use.
#' @param out character scalar with path to save the \code{aspell} word list
#' @keywords internal
generate_spelling <- function(out = ".aspell/words.pws") {
  proper_nouns <- c("Wasey", "Quan", "Walraven", "Deyo", "wmurphyrd", "Elixhauser")
  proper_nouns <- c(proper_nouns, paste(proper_nouns, "'s", sep = ""))
  acronyms <- c("AHRQ", "BISHCA", "CRAN", "CDC", "CMS",
                "POA", "HTN", "ICD",
                "RTF", "SAS", "VAHHS", "NSO")
  other <- c("comorbidity", "comorbidities", "comorbid",
             "unclassifiable", "requestor", "Puerperium")
  # "requestor" in Vermont data licence
  words <- c(proper_nouns, acronyms, other)
  aspell_write_personal_dictionary_file(words, out = out)
  invisible(words)
}
