# This is a bit painful, but defining known spellings of names and technical terms makes the aspell checks useful (and removes a NOTE in the R CMD check which at least some of the CRAN people use.

proper_nouns <- c("Wasey", "Quan", "Walraven", "Deyo", "wmurphyrd", "Elixhauser")
proper_nouns <- c(proper_nouns, paste(proper_nouns, "'s", sep = ""))

acronyms <- c("POA", "ICD", "CRAN", "CMS", "AHRQ", "SAS")

other <- c("comorbidity", "comorbidities", "comorbid", "unclassifiable")

aspell_write_personal_dictionary_file(c(proper_nouns, acronyms, other), out = ".aspell/words.pws")

