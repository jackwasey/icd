.icd9_generate_subchap_lookup <- function() {
  .icd9_generate_chap_lookup(
    chapters = icd::icd9_sub_chapters,
    prefix = "sc"
  )
}

.icd10_generate_subchap_lookup <- function() {
  .icd10_generate_chap_lookup(
    chapters = icd::icd10_sub_chapters,
    prefix = "sc"
  )
}

.icd9_generate_chap_lookup <- function(chapters = icd::icd9_chapters,
                                       prefix = "chap") {
  stopifnot(is.list(chapters), is.character(prefix))
  df_rows <- lapply(
    names(chapters),
    function(nm) {
      chap <- chapters[[nm]]
      data.frame(
        expand_range_major.icd9(
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

.icd10_generate_chap_lookup <- function(chapters = icd::icd10_chapters,
                                        prefix = "chap") {
  stopifnot(is.list(chapters), is.character(prefix))
  df_rows <- lapply(
    names(chapters),
    function(nm) {
      chap <- chapters[[nm]]
      .trc("icd10 chap lookup: ", paste(chap, collapse = ", "))
      data.frame(
        expand_range_major.icd10cm(
          as.icd10cm(unname(chap["start"])),
          as.icd10cm(unname(chap["end"])),
          defined = FALSE
        ),
        nm
      )
      # TODO: ICD-10 is not in leixcographic order. E.g., C7A is not in the same group
      # as C76-C80. Sort here?
    }
  )
  chap_lookup <- do.call(rbind, df_rows)
  names(chap_lookup) <- c(
    paste0(prefix, "_major"),
    paste0(prefix, "_desc")
  )
  chap_lookup
}
