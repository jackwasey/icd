#' Generate spelling exceptions
#'
#' This is a bit painful, but defining known spellings of names and technical
#' terms makes the \code{aspell} checks useful (and removes a NOTE in the
#' \code{R CMD check} which at least some of the CRAN people use.
#' @param out character scalar with path to save the \code{aspell} word list
#' @template save_data
#' @keywords internal
generate_spelling <- function(out = ".aspell/words.pws", save_data = TRUE) {
  medical <- c("arrythmia", "Musculoskeletal", "Myocardial",
               "Puerperium", "laterality", "Infarction")
  proper_nouns <- c("Charlson", "Deyo", "Elixhauser", "Mansour", "OpenMP", "Quan",
                    "Sharabiani", "Travis", "Walraven", "Wasey", "Wercker", "Wickham",
                    "wmurphyrd")
  proper_nouns <- c(proper_nouns, paste(proper_nouns, "'s", sep = ""))
  acronyms <- c("AHRQ", "BISHCA", "CRAN", "CDC", "CMS", "CSV", "DRG",
                "POA", "HTN", "ICD",
                "RTF", "SAS", "VAHHS", "NSO", "WSU")
  other <- c("comorbidity", "comorbidities", "comorbid",
             "unclassifiable", "requestor", "Transuranium", "TODO",
             "MERCHANTABILITY", "prepend", "prepends")
  # "requestor" in Vermont data licence
  words <- c(medical, proper_nouns, acronyms, other)
  if (save_data)
    utils::aspell_write_personal_dictionary_file(words, out = out)
  invisible(words)
}
