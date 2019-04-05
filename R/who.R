#' Functions to get the WHO ICD-10 2016 data
#'
#' This is likely to be not exported in the future, as it is designed for the
#' transition to icd.data version 1.1 or its removal. The user may use
#' \code{\link{get_icd10who2016}} as if it is a variable. In some situations, it may be
#' preferable to call this function.
#' @param resource Fragment of URL with specific ICD-10 resource requested
#' @param edition icd10
#' @param year Four-digit year as integer or character
#' @template lang
#' @template verbose
#' @return
#' \code{.who_api} returns the JSON data, or fails with NULL
#' @keywords internal datasets
#' @noRd
.who_api <- function(resource,
                     edition = "icd10",
                     year = 2016,
                     lang = "en",
                     offline = .offline(),
                     verbose = .verbose()) {
  httr_retry <- httr::RETRY
  if (.have_memoise()) {
    httr_retry <- memoise::memoise(
      httr_retry,
      cache = memoise::cache_filesystem(
        file.path(icd_data_dir(), "memoise")
      )
    )
  }
  edition <- match.arg(edition)
  who_base <- "https://apps.who.int/classifications"
  json_url <- paste(who_base, edition, "browse", year, lang, resource, sep = "/")
  # TODO: this stops us using the memoised data, even if available. Seems an unlikely situation, except maybe for local testing. memoise::has_cache(f, ...) lets us test whether a memoise call is cached already.
  if (.offline() || !.interact()) {
    msg <- "Offline and not interactive, so not attempting WHO data download."
    .absent_action_switch(msg)
    return(NULL)
  }
  if (verbose > 1) message("Getting WHO data with JSON: ", json_url)
  http_response <- httr_retry("GET", json_url)
  if (hs <- http_response$status_code >= 400) {
    warning("Unable to fetch resource: ", json_url, " has HTTP status, ", hs)
    return()
  }
  json_data <- rawToChar(http_response$content)
  jsonlite::fromJSON(json_data)
}

#' Use WHO API to discover chapters
#'
#' Of note, the \code{WHO} package does not provide access to classifications, just
#' WHO summary data.
#' @keywords internal
#' @noRd
.who_api_chapter_names <- function(ver = "icd10",
                                   year = 2016,
                                   lang = "en",
                                   verbose = .verbose()) {
  .who_api_children(
    ver = ver,
    year = year,
    lang = lang,
    verbose = verbose
  )[["label"]]
}

.who_api_children <- function(concept_id = NULL, ...) {
  if (is.null(concept_id)) {
    .who_api(resource = "JsonGetRootConcepts?useHtml=false", ...)
  } else {
    .who_api(
      resource = paste0(
        "JsonGetChildrenConcepts?ConceptId=",
        concept_id,
        "&useHtml=false"
      ), ...
    )
  }
}

#' Use public interface to fetch ICD-10 WHO data for a given version
#'
#' The user may call this function to install the full WHO ICD-10 definition on
#' their machine, after which it will be available to \code{icd}. TODO:
#' determine the best place to save this data.
#' @param concept_id This is the id for the code or code group, e.g.,
#'   \sQuote{XI} (Chapter 6), \sQuote{T90--T98} (A sub-chapter), \sQuote{E01} (A
#'   sub-sub-chapter). You cannot query a single code with this interface.
#' @param year integer 4-digit year
#' @param lang Currently it seems only 'en' works
#' @param verbose logical
#' @param ... further arguments passed to self recursively, or \code{.who_api}
#' @keywords internal
#' @noRd
.dl_icd10who <- function(concept_id = NULL,
                         year = 2016,
                         lang = "en",
                         progress = TRUE,
                         verbose = .verbose(),
                         hier_code = character(),
                         hier_desc = character(),
                         offline = .offline(),
                         ...) {
  if (verbose > 1) print(hier_code)
  if (verbose > 1) message(".who_api_tree with concept_id = ", concept_id)
  if (offline) {
    if (verbose) message("Returning NULL because offline")
    return()
  }
  tree_json <- .who_api_children(
    concept_id = concept_id,
    year = year,
    lang = lang,
    verbose = verbose,
    ...
  )
  if (is.null(tree_json)) {
    warning(
      "Unable to retrieve results for concept_id: ", concept_id,
      "so returning NULL. Try re-running the command."
    )
    return()
  }
  if (verbose > 1) message("hier level = ", length(hier_code))
  new_hier <- length(hier_code) + 1
  # parallel mcapply is about 2-3x as fast, but may get throttled for multiple
  # connections. It seems to get up to about 10-15, which is reasonable.
  all_new_rows <- parallel::mclapply(
    seq_len(nrow(tree_json)),
    function(branch) {
      new_rows <- data.frame(
        code = character(),
        leaf = logical(),
        desc = character(),
        three_digit = character(),
        major = character(),
        sub_sub_chapter = character(),
        sub_chapter = character(),
        chapter = character()
      )
      # might be looping through chapters, sub-chapters, etc.
      child_code <- tree_json[branch, "ID"]
      child_desc <- tree_json[branch, "label"]
      is_leaf <- tree_json[branch, "isLeaf"]
      # for each level, if not defined by arguments, then assign next possible
      hier_code[new_hier] <- child_code
      hier_desc[new_hier] <- child_desc
      sub_sub_chapter <- NA
      hier_three_digit_idx <- which(nchar(hier_code) == 3 &
        !grepl("[XVI-]", hier_code))
      if (length(hier_code) >= 3 && nchar(hier_code[3]) > 3) {
        sub_sub_chapter <- hier_desc[3]
      }
      this_child_up_hier <- grepl("[XVI-]", child_code)
      three_digit <- hier_code[hier_three_digit_idx]
      major <- hier_desc[hier_three_digit_idx]
      if (!this_child_up_hier && !is.na(three_digit)) {
        # TODO: consider add the chapter, subchapter codes
        new_item <- data.frame(
          code = child_code,
          leaf = is_leaf,
          desc = child_desc,
          three_digit = three_digit,
          major = major,
          sub_sub_chapter = sub_sub_chapter,
          sub_chapter = hier_desc[2],
          chapter = hier_desc[1],
          stringsAsFactors = FALSE
        )
        stopifnot(child_code %nin% new_rows$code)
        new_rows <- rbind(new_rows, new_item)
      }
      if (!is_leaf) {
        if (verbose > 1) message("Not a leaf, so recursing")
        if (progress) cat(".")
        recursed_rows <- .dl_icd10who(
          concept_id = child_code,
          year = year,
          lang = lang,
          verbose = verbose,
          hier_code = hier_code,
          hier_desc = hier_desc,
          ...
        )
        stopifnot(!any(recursed_rows$code %in% new_rows$code))
        new_rows <- rbind(new_rows, recursed_rows)
      } # not leaf
      new_rows
    }
  ) # loop
  if (verbose > 1) {
    message(
      "leaving recursion with length(all_new_rows) = ",
      length(all_new_rows)
    )
  }
  # just return the rows (we are recursing so can't save anything in this function). Parser can do this.
  do.call(rbind, all_new_rows)
}

.dl_icd10who_finalize <- function(dat, year, lang) {
  rownames(dat) <- NULL
  dat[["code"]] <- sub(pattern = "\\.", replacement = "", x = dat[["code"]])
  for (col_name in c(
    "chapter",
    "sub_chapter",
    "sub_sub_chapter",
    "major",
    "desc"
  ))
    dat[[col_name]] <- sub("[^ ]+ ", "", dat[[col_name]])
  var_name <- paste0("icd10who", year, ifelse(lang == "en", "", lang))
  .save_in_resource_dir(var_name, x = dat)
  # First, if three digit doesn't match code, then drop the row, as these are incorrectly assimilated rows.
  thr <- get_major.icd10(dat$code)
  dat <- dat[dat$three_digit == thr, ]
  # Then I think any remaining rows are plain duplicates
  dat[!duplicated(dat$code), ]
}

.parse_icd10who2016 <- function(...) {
  if (!.confirm_download()) return()
  .dl_icd10who_finalize(
    .dl_icd10who(year = 2016, lang = "en", ...),
    2016, "en"
  )
}

.parse_icd10who2008fr <- function(...) {
  if (!.confirm_download()) return()
  .dl_icd10who_finalize(
    .dl_icd10who(year = 2008, lang = "fr", ...),
    2008, "fr"
  )
}

.downloading_who_message <- function() {
  message("Downloading or parsing cached WHO ICD data. This may take a few minutes. Data is cached, so if there is a download error, repeating the instruction will return the data immediately if cached, or pick up where it left off.") # nolint
}
