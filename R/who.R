# TODO: only download (and cache) WHO data as needed, rather than forcing user to wait minutes to download everything on first use.

#' Functions to get the WHO ICD-10 English 2016 and French 2008 data
#' @param resource Fragment of URL with specific ICD-10 resource requested
#' @param year Four-digit year as integer or character
#' @template lang
#' @return
#' \code{.dl_icd10who_memoise} returns the JSON data, or fails with NULL
#' @keywords internal datasets
#' @noRd
.dl_icd10who_memoise <- function(resource,
                                 year = 2016,
                                 lang = "en") {
  # WHO changed the URL from https://apps.who.int/classifications to
  # https://icd.who.int/browse10 . Nothing complicated: I set this (if unset) in
  # zzz.R on package load. If there is another change, the user can update this
  # with a package update.

  # memoise package has given me problems and crashes. DIY
  mem_file_name <- paste(
    "WHO", year, lang,
    gsub("JsonGetChildrenConcepts\\?ConceptId=|&useHtml=false", "", resource),
    "json",
    sep = "."
  )
  mem_dir <- file.path(get_icd_data_dir(), "memoise")
  dir.create(mem_dir, showWarnings = FALSE)
  mem_path <- file.path(mem_dir, mem_file_name)
  if (file.exists(mem_path)) {
    .trc(paste(
      "Have memoised data for ", year, lang, resource,
      "from", mem_path
    ))
    readRDS(mem_path)
  } else {
    res <- .dl_icd10who_json(year, lang, resource)
    .trc(paste(
      "Saving memoised data for ", year, lang, resource,
      "in", mem_path
    ))
    saveRDS(res, mem_path, version = 2)
    res
  }
}

.dl_icd10who_json <- function(year, lang, resource) {
  json_url <- paste(
    getOption("icd.data.who_url"),
    year,
    lang,
    resource,
    sep = "/"
  )
  if (.offline() && !.interact()) {
    msg <- "Offline and not interactive, so not attempting WHO data download."
    .absent_action_switch(msg)
    return(NULL)
  }
  .msg("Getting WHO data with JSON: ", json_url)
  http_response <- httr::RETRY("GET", json_url)
  if (hs <- http_response$status_code >= 400) {
    .msg("trying once more")
    http_response <- httr::RETRY("GET", json_url)
    if (hs <- http_response$status_code >= 400) {
      stop(
        "Unable to fetch resource: ", json_url,
        " with HTTP status, ", hs, ". Check your internet connection, ",
        "retry later, then file an issue at: ",
        "https://github.com/jackwasey/icd/issues ."
      )
    }
  } # end 400+
  json_data <- rawToChar(http_response$content)
  jsonlite::fromJSON(json_data)
}

#' Use WHO API to discover chapters
#'
#' Of note, the \code{WHO} package does not provide access to classifications, just
#' WHO summary data.
#' @keywords internal
#' @noRd
.dl_icd10who_chapter_names <- function(ver = "icd10",
                                       year = 2016,
                                       lang = "en") {
  .dl_icd10who_children(
    ver = ver,
    year = year,
    lang = lang
  )[["label"]]
}

#' Get the children of a concept (ICD-10 chapter, code or range)
#' @param concept_id NULl for root, concept string for any leaf or intermediate.
#' @examples
#' .dl_icd10who_children("XXII")
#' .dl_icd10who_children("U84")
#' # U85 is a leaf node, returns no children as empty list
#' .dl_icd10who_children("U82-U85")
#' # https://icd.who.int/browse10/2016/en#/U85
#' .dl_icd10who_children("U85")
#' # https://icd.who.int/browse10/2016/en#/P90
#' .dl_icd10who_children("P90-P96")
#' .dl_icd10who_children("P90")
#' @keywords internal
#' @noRd
.dl_icd10who_children <- function(concept_id = NULL, ...) {
  resource <- if (is.null(concept_id)) {
    "JsonGetRootConcepts?useHtml=false"
  } else {
    paste0(
      "JsonGetChildrenConcepts?ConceptId=",
      concept_id,
      "&useHtml=false"
    )
  }
  .dl_icd10who_memoise(resource = resource, ...)
}

#' Use public interface to fetch ICD-10 WHO data for a given version
#'
#' The user may call this function to install the full WHO ICD-10 definition on
#' their machine, after which it will be available to \CRANpkg{icd}.
#' @param concept_id This is the id for the code or code group, e.g.,
#'   \sQuote{XI} (Chapter 6), \sQuote{T90--T98} (A sub-chapter), \sQuote{E01} (A
#'   sub-sub-chapter). You cannot query a single code with this interface.
#' @param year integer 4-digit year
#' @param lang Currently it seems only 'en' works
#' @param ... further arguments passed to self recursively, or
#'   \code{.dl_icd10who_memoise}
#' @examples
#' \dontrun{
#' .dl_icd10who_walk(year = 2016, lang = "en", concept_id = "B20-B24")
#' }
#' @keywords internal
#' @noRd
.dl_icd10who_walk <- function(concept_id = NULL,
                              year = 2016,
                              lang = "en",
                              hier_code = character(),
                              hier_desc = character(),
                              ...) {
  .dbg(
    ".dl_icd10who_memoise with concept_id = ",
    ifelse(is.null(concept_id), "NULL", concept_id)
  )
  .dbg(paste(hier_code, collapse = " -> "))
  if (.offline()) {
    .msg("Returning NULL because offline")
    return()
  }
  tree_json <- .dl_icd10who_children(
    concept_id = concept_id,
    year = year,
    lang = lang,
    ...
  )
  if (is.null(tree_json)) {
    warning(
      "Unable to retrieve results for concept_id: ", concept_id,
      "so returning NULL. Try re-running the command."
    )
    return()
  }
  .dbg("hier level = ", length(hier_code))
  new_hier <- length(hier_code) + 1
  # parallel mclapply is about 2-3x as fast, but may get throttled for multiple
  # connections, and error handling and debugging is much harder.
  all_new_rows <- lapply(
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
      re_chap_or_sub_chap <- "(^[XVI]+$)|(^.+-.+$)"
      hier_three_digit_idx <- which(nchar(hier_code) == 3 &
        !grepl(re_chap_or_sub_chap, hier_code))
      if (length(hier_code) >= 3 && nchar(hier_code[3]) > 3) {
        sub_sub_chapter <- hier_desc[3]
      }
      this_child_up_hier <- grepl(re_chap_or_sub_chap, child_code)
      three_digit <- hier_code[hier_three_digit_idx]
      major <- hier_desc[hier_three_digit_idx]
      if (!this_child_up_hier && !is.na(three_digit)) {
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
        .dbg(
          paste(new_rows$code, collapse = ", "),
          " not a leaf, so recursing"
        )
        recursed_rows <- .dl_icd10who_walk(
          concept_id = child_code,
          year = year,
          lang = lang,
          hier_code = hier_code,
          hier_desc = hier_desc,
          ...
        )
        stopifnot(!any(recursed_rows$code %in% new_rows$code))
        new_rows <- rbind(new_rows, recursed_rows)
      } # not leaf
      new_rows
    }
  ) # lapply loop
  if (.verbose() > 1) {
    .dbg(
      "leaving recursion with length(all_new_rows) = ",
      length(all_new_rows)
    )
    if (length(all_new_rows$code)) {
      .trc(paste(all_new_rows$code, collapse = ", "), print = TRUE)
    }
  }
  # just return the rows (we are recursing so can't save anything in this
  # function). Parser can do this.
  if (!all(vapply(all_new_rows, is.data.frame, logical(1))) ||
    !all(vapply(all_new_rows, ncol, integer(1)) == ncol(all_new_rows[[1]]))
  ) {
    stop(
      "Error when downloading WHO ICD data. ",
      "(Concept ID = ", concept_id, ") ",
      "This may be a temporary download failure. Please re-try the command."
    )
  }
  do.call(rbind, all_new_rows)
}

.dl_icd10who_finalize <- function(dat, year, lang) {
  dat[["code"]] <- sub(pattern = "\\.", replacement = "", x = dat[["code"]])
  for (col_name in c(
    "chapter",
    "sub_chapter",
    "sub_sub_chapter",
    "major",
    "desc"
  )) {
    dat[[col_name]] <- sub("[^ ]+ ", "", dat[[col_name]])
  }
  # First, if three digit doesn't match code, then drop the row, as these are
  # incorrectly assimilated rows.
  thr <- get_major.icd10(dat$code)
  dat <- dat[dat$three_digit == thr, ]
  dat$three_digit <- factor_sorted_levels(as.icd10who(dat$three_digit))
  # Then I think any remaining rows are plain duplicates
  dat <- dat[!duplicated(dat$code), ]
  dat <- dat[order(dat$code), ]
  rownames(dat) <- NULL
  var_name <- paste0("icd10who", year, ifelse(lang == "en", "", lang))
  dat$code <- as.icd10who(dat$code)
  .save_in_resource_dir(var_name, x = dat)
  invisible(dat)
}

.parse_icd10who2016 <- function(...) {
  if (!.confirm_download()) return()
  .dl_icd10who_finalize(
    .dl_icd10who_walk(year = 2016, lang = "en", ...),
    2016, "en"
  )
}

.parse_icd10who2008fr <- function(...) {
  if (!.confirm_download()) return()
  .dl_icd10who_finalize(
    .dl_icd10who_walk(year = 2008, lang = "fr", ...),
    2008,
    "fr"
  )
}

.downloading_who_message <- function() {
  message("Downloading or parsing cached WHO ICD data. This may take a few minutes. Data is cached, so if there is a download error, repeating the instruction will return the data immediately if cached, or pick up where it left off.") # nolint
}
