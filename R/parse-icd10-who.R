# Obtain WHO ICD-10 codes, experimental, incomplete.

#nocov start

#' Get ICD-10 (not ICD-10-CM) as published by CDC
#'
#' @details There is no copyright notice, and, as I understand it, by default US government publications are public
#'   domain ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD10/ and thus this or derivative data can be
#'   included in the package distribution
#' @keywords internal
icd10_get_who_from_cdc <- function() {
  # beware, not all download.file methods can handle %20 etc in URLs correctly.
  url <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD10/allvalid2011%20%28detailed%20titles%20headings%29.txt"
  file_path <- download_to_data_raw(url = url)$file_path

  # typically, the file isn't easily machine readable with stupidly placed
  # annotations, e.g. "Added in 2009	A09.9	Gastroenteritis and colitis of
  # unspecified origin" I've no idea what those people are thinking when they do
  # this kind of thing.

  # ignore locale issue right now. This set has a lot of the different cases: dat[70:75,]
  readr::read_lines(file_path, skip = 7) %>%
    str_trim() %>%
    str_match("(.*\\t)?(.+)\\t+(.+)") -> dat

  code_or_range <- dat[, 3]
  desc <- dat[, 4]

  # this data set does not explicitly say which codes are leaves or parents.
  is_range <- str_detect(code_or_range, "-")
  # this is a mix of chapters and sub-chapters, and would require processing to
  # figure out which

  codes <- dat[!is_range, 3]
  codes_desc <- dat[!is_range, 4]

  class(codes) <- c("icd10who", "icd10", "character")
  # do some sanity checks:
  stopifnot(all(icd_is_valid(codes)))

  #> codes[!icd_is_valid(codes)]
  #[1] "*U01"   "*U01.0" "*U01.1" "*U01.2" "*U01.3" "*U01.4" "*U01.5" "*U01.6" "*U01.7" "*U01.8" "*U01.9" "*U02"   "*U03"   "*U03.0"
  #[15] "*U03.9" NA

  stop("work in progress", codes_desc, desc)
}

#' scrape WHO web site for ICD-10 codes
#'
#' javascript only (at least in recent years), so can't just get the HTML.
#' Thanks guys.
#'
#' @details PhantomJS is not required: it can drive a regular firefox browsing
#'   session directly, although this is not headless, it doesn't crash all the
#'   time like phantomjs. TODO still: daggers and asterisks after codes. Tests.
#'   Sorting and structuing output, probably in a denormalized data frame with
#'   columns: code, (short desc avail?), desc, (notes, e.g. exclusions?), major
#'   (the parent major code), sub_chapter, chapter. This should be like the
#'   icd9cm, if possible.
#'
#'   Do more sanity checks and testing early on, e.g. for invalid codes, unusual
#'   characters, vector lengths
#' @keywords internal
scrape_icd10_who <- function(debug = FALSE, verbose = FALSE, silent = FALSE) {
  #library("RJSONIO") # this seems to avoid a lot of errors?
  loadNamespace("RSelenium")
  loadNamespace("xml2")

  if (debug)
    RSelenium::startServer(invisible = FALSE, log = FALSE)
  else
    RSelenium::startServer()

  selenium_driver <- RSelenium::remoteDriver(
    extraCapabilities = list(webdriver.firefox.bin = "C:\\FirefoxCollection\\Mozilla Firefox 36.0\\firefox.exe")
  )
  selenium_driver$open()
  # make sure we always wait for the page to load (or ten seconds), before returning
  #selenium_driver$setTimeout(type = "page load", milliseconds = 10000)
  #selenium_driver$setTimeout(type = "script", milliseconds = 10000)
  #selenium_driver$setTimeout(type = "implicit", milliseconds = 10000)
  selenium_driver[["setImplicitWaitTimeout"]](milliseconds = 10000)

  who_icd10_url_base <- "http://apps.who.int/classifications/icd10/browse/2016/en#/"

  chapter_urls <- paste0(who_icd10_url_base, as.roman(1:21))

  all_sub_chapters <- list()
  all_majors <- list()
  all_leaves <- list()

  for (chapter_url in chapter_urls) {
    if (!silent) message(chapter_url)
    selenium_driver$navigate(chapter_url)
    #chapter_html <- selenium_driver$getPageSource()

    # if (debug)
    #   browser(expr = length(chapter_html) != 1)
    # else
    #   stopifnot(length(chapter_html) == 1)
    # chapter_xml <- xml2::read_html(chapter_html[[1]])

    # if (debug)
    #   browser(expr = length(chapter_xml) == 0)
    # else
    #   stopifnot(length(chapter_xml) > 0)

    # instead of querying via phantomjs (which crashes all the time), get the
    # whole document, then use xml2 and rvest:

    selenium_driver$findElements(using = "xpath", "//li[@class='Blocklist1']") %>%
      vapply(function(x) unlist(x[["getElementText"]]()), character(1)) %>%
      str_trim() %>%
      str_replace_all("[[:space:]]+", " ") %>%
      str_pair_match("([^[:space:]]+) (.+)", swap = TRUE) %>%
      lapply(
        function(x) str_split(x, "-") %>%
          unlist %>%
          set_names(c("start", "end"))
      ) -> sub_chapters

    all_sub_chapters <- c(all_sub_chapters, sub_chapters)

    # next, look at individual heading and leaf (billing) codes
    # this can be accomplished using constructed URLs, also, e.g.:
    # http://apps.who.int/classifications/icd10/browse/2016/en#/A92-A99


    # now in a new loop, we can generate the drilled down URLs from the subchapter
    # ranges without mucking around by 'clicking' on links
    #  e.g. http://apps.who.int/classifications/icd10/browse/2016/en#/H40-H42
    #   there are "Category 1" elements which are equivalent to 'major' types, e.g. H40 Glaucoma
    # <h4>
    # <a name="H40" id="H40" class="code">H40</a>
    # <span class="label">Glaucoma</span>
    # </h4>

    #   and "category 2" elements which are the leaf nodes. e.g. H40.3 Glaucoma secondary to eye trauma
    # <h5>
    # <a name="H40.0" id="H40.0" class="code">H40.0</a>
    # <span class="label">Glaucoma suspect</span>
    # </h5>


    for (sub_chapter in sub_chapters) {
      sub_chapter_url <- paste0(who_icd10_url_base, sub_chapter["start"], "-", sub_chapter["end"])
      if (verbose && !silent) message(sub_chapter_url)

      selenium_driver$navigate(sub_chapter_url)
      Sys.sleep(0.5)

      # new way
      selenium_driver$findElements(using = "xpath", "//div[@class='Category1']//a[@class='code']") %>%
        vapply(function(x) unlist(x[["getElementText"]]()), character(1)) %>%
        str_trim() -> majors

      selenium_driver$findElements(using = "xpath", "//div[@class='Category1']//span[@class='label']") %>%
        vapply(function(x) unlist(x[["getElementText"]]()), character(1)) %>%
        str_trim()  %>%
        str_replace_all("[[:space:]]+", " ") -> majors_desc


      selenium_driver$findElements(using = "xpath", "//div[@class='Category2']//a[@class='code']") %>%
        vapply(function(x) unlist(x[["getElementText"]]()), character(1)) %>%
        str_trim() -> leaves

      selenium_driver$findElements(using = "xpath", "//div[@class='Category2']//span[@class='label']") %>%
        vapply(function(x) unlist(x[["getElementText"]]()), character(1)) %>%
        str_trim()  %>%
        str_replace_all("[[:space:]]+", " ") -> leaves_desc


      #sub_chapter_html <- selenium_driver$getPageSource()

      #sub_chapter_xml <- xml2::read_html(sub_chapter_html[[1]])

      # sub_chapter_xml %>%
      #   xml2::xml_find_all("//div[@class='Category1']//a[@class='code']") %>%
      #   xml2::xml_text() %>%
      #   str_trim() -> majors
      #
      # sub_chapter_xml %>%
      #   xml2::xml_find_all("//div[@class='Category1']//span[@class='label']") %>%
      #   xml2::xml_text() %>%
      #   str_trim() %>%
      #   str_replace_all("[[:space:]]+", " ") -> majors_desc

      # sanity check
      if (debug)
        browser(expr = length(majors) != length(majors_desc))
      else
        stopifnot(length(majors) == length(majors_desc))

      names(majors) <- majors_desc

      # sub_chapter_xml %>%
      #   xml2::xml_find_all("//div[@class='Category2']//a[@class='code']") %>%
      #   xml2::xml_text() %>%
      #   str_trim() -> leaves
      #
      # sub_chapter_xml %>%
      #   xml2::xml_find_all("//div[@class='Category2']//span[@class='label']") %>%
      #   xml2::xml_text() %>%
      #   str_trim() %>%
      #   str_replace_all("[[:space:]]+", " ") -> leaves_desc

      if (debug)
        browser(expr = length(leaves) != length(leaves_desc))
      else
        stopifnot(length(leaves) == length(leaves_desc))

      names(leaves) <- leaves_desc

      # someday, add the exclusion rubric to the data structure for detailed validation

      all_majors <- c(all_majors, majors)
      all_leaves <- c(all_leaves, leaves)

      if (debug) {
        print(tail(majors))
        print(tail(leaves))
      }
    }
  }
  selenium_driver$close()

  icd10_who_sub_chapters <- all_sub_chapters
  icd10_who_majors <- all_majors
  icd10_who_leaves <- all_leaves

  # temporary saves for testing:
  save_in_data_dir(icd10_who_sub_chapters)
  save_in_data_dir(icd10_who_majors)
  save_in_data_dir(icd10_who_leaves)

  # combine into big data frame like icd9Hierarchy:
  #names(icd9Hierarchy)
  #[1] "code"       "short_desc"  "long_desc"   "three_digit" "major"      "sub_chapter" "chapter"

  #> head(icd9Hierarchy, 1)
  #code short_desc long_desc three_digit   major                     sub_chapter                           chapter
  #001   Cholera  Cholera        001 Cholera Intestinal Infectious Diseases Infectious And Parasitic Diseases

  icd10_who_hierarchy <- data.frame(
    icd10 = character(),
    desc_long = character(),
    major = character(),
    sub_chapter = character(),
    chapter = character(),
    major_desc = character(),
    sub_chapter_desc = character(),
    chapter_desc = character()
  )
  save_in_data_dir(icd10_who_hierarchy)
}
#nocov end
