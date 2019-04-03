.icd9_generate_subchap_lookup <- function() {
  .icd9_generate_chap_lookup(
    chapters = icd9_sub_chapters,
    prefix = "sc"
  )
}

.icd10_generate_subchap_lookup <- function() {
  .icd10_generate_chap_lookup(
    chapters = icd10_sub_chapters,
    prefix = "sc"
  )
}

.icd9_generate_chap_lookup <-
  function(chapters = icd9_chapters,
             prefix = "chap") {
    stopifnot(is.list(chapters), is.character(prefix))
    df_rows <- lapply(
      names(chapters),
      function(nm) {
        chap <- chapters[[nm]]
        data.frame(
          .erm9(
            as.icd9cm(chap["start"]),
            as.icd9cm(chap["end"]),
            defined = FALSE
          ),
          nm
        )
      }
    )
    chap_lookup <- do.call(rbind, df_rows)
    names(chap_lookup) <- c(
      paste0(prefix, "_major"),
      paste0(prefix, "_desc")
    )
    chap_lookup
  }

.icd10_generate_chap_lookup <-
  function(chapters = icd10_chapters,
             prefix = "chap") {
    stopifnot(is.list(chapters), is.character(prefix))
    df_rows <- lapply(
      names(chapters),
      function(nm) {
        chap <- chapters[[nm]]
        data.frame(
          .erm10(
            as.icd10cm(chap["start"]),
            as.icd10cm(chap["end"]),
            defined = FALSE
          ),
          nm
        )
        # ICD-10 is not in leixcographic order. E.g., C7A is not in the same group
        # as C76-C80
      }
    )
    chap_lookup <- do.call(rbind, df_rows)
    names(chap_lookup) <- c(
      paste0(prefix, "_major"),
      paste0(prefix, "_desc")
    )
    chap_lookup
  }
