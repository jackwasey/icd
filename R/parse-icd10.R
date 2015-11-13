
#' get all ICD-10-CM codes
#'
#' gets all ICD-10-CM codes from an archive on the CDC web site at \url{http://www.cdc.gov/nchs/data/icd/icd10cm/2016/ICD10CM_FY2016_code_descriptions.zip}. Initially, this just grabs 2016.
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_trim
#' @references https://www.cms.gov/Medicare/Coding/ICD10/downloads/icd-10quickrefer.pdf
#' @keywords internal
icd10cm_get_all_real <- function(save = TRUE) {

  local_path <- unzip_to_data_raw(
    url = "http://www.cdc.gov/nchs/data/icd/icd10cm/2016/ICD10CM_FY2016_code_descriptions.zip",
    file_name = "icd10cm_order_2016.txt")

  raw <- readLines(con = local_path)
  icd10cm2016 <- data.frame(id = substr(raw, 1, 5),
                            code = substr(raw, 7, 13),
                            leaf = substr(raw, 14, 15),
                            descShort = substr(raw, 16, 76),
                            descLong = substr(raw, 77, stop = 1e5),
                            stringsAsFactors = FALSE
  )

  icd10cm2016 <- as.data.frame(lapply(icd10cm2016, stringr::str_trim), stringsAsFactors = FALSE)
  if (save) save_in_data_dir(icd10cm2016)
  return(invisible(icd10cm2016))

  # now some test code to see what permutations there are of ICD-10 codes based
  # on the 2016 CM set.
  #i10 <- icd10cm2016$code

  #alpha_in_tail <- grep("[[:alpha:]]", i10tail, value = TRUE)
  #alpha_in_tail_bool <- grepl("[[:alpha:]].*[[:alpha:]].*", x = i10)
  #alpha_in_tail <- i10[alpha_in_tail_bool]
  #unique(gsub("[[:digit:]]", replacement = "", x = alpha_in_tail))

  # verify, e.g. J in middle?
  #grep("[[:alpha::]].*J.*", i10)

  # find unique characters at each position from 4 to 7
  # for (i in 1:7)
  #   message(i)
  #   substring(alpha_in_tail, i, i) %>% unique %>% sort %>% message
  # }
}

get_phantom <- function(extras = c("--load-images=false", "--disk-cache=true")) {
  if (Sys.info()[["sysname"]] == "Windows")
    phantomjs <- RSelenium::phantom(pjs_cmd = "tools/phantomjs.exe", extras = extras)
  else
    phantomjs <- RSelenium::phantom(extras = extras)
}

#' scrape WHO web site for ICD-10 codes
#'
#' javascript only (at least in recent years), so can't just get the HTML.
#' Thanks guys.
#'
#' also requires phatomjs to render the javascript
#' @import RSelenium xml2
#' @keywords internal
scrape_icd10_who <- function(sleep_secs = 0) {
  library("RSelenium")
  library("magrittr")
  library("xml2")
  #requireNamespace("rvest")

  phantomjs <- get_phantom()
  on.exit(phantomjs$stop())
  Sys.sleep(sleep_secs)

  eCap <- list(phantomjs.page.settings.userAgent
               = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:29.0) Gecko/20120101 Firefox/29.0")
  #selenium_driver <- RSelenium::remoteDriver(
    #browserName = "phantomjs",
    #extraCapabilities = eCap)
  #RSelenium::checkForServer()
  RSelenium::startServer()
  selenium_driver <- RSelenium::remoteDriver(browserName = "chrome")
  selenium_driver$open()

  who_icd10_url_base <- "http://apps.who.int/classifications/icd10/browse/2016/en#/"
  chapter_urls <- paste0(who_icd10_url_base, as.roman(1:21))

  all_sub_chapters <- list()
  all_majors <- list()
  all_leaves <- list()


  for (chapter_url in chapter_urls) {
    message(chapter_url)
    Sys.sleep(sleep_secs)
    # selenium_driver$open()
    Sys.sleep(sleep_secs)
    selenium_driver$navigate(chapter_url)
    Sys.sleep(sleep_secs)
    chapter_html <- selenium_driver$getPageSource()
    # selenium_driver$close()

    stopifnot(length(chapter_html) == 1)
    chapter_xml <- xml2::read_html(chapter_html[[1]])

    stopifnot(length(chapter_xml) > 0)

    # instead of querying via phantomjs (which crashes all the time), get the
    # whole document, then use xml2 and rvest:

    chapter_xml %>%
      xml2::xml_find_all("//li[@class='Blocklist1']") %>%
      xml2::xml_text() %>%
      stringr::str_trim() %>%
      stringr::str_replace_all("[[:space:]]+", " ") %>%
      str_pair_match("([^[:space:]]+) (.+)", swap = TRUE) %>%
      lapply(
        function(x) stringr::str_split(x, "-") %>%
          unlist %>%
          magrittr::set_names(c("start", "end"))
      ) -> sub_chapters

    all_sub_chapters <- c(all_sub_chapters, sub_chapters)

    message("got sub_chapters")

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
      message(sub_chapter_url)
      Sys.sleep(sleep_secs)
      #selenium_driver$open()
      #Sys.sleep(sleep_secs)
      selenium_driver$navigate(sub_chapter_url)
      Sys.sleep(sleep_secs)
      sub_chapter_html <- selenium_driver$getPageSource()
      #Sys.sleep(sleep_secs)
      #selenium_driver$close()

      sub_chapter_xml <- xml2::read_html(sub_chapter_html[[1]])

      sub_chapter_xml %>%
        xml2::xml_find_all("//div[@class='Category1']//a[@class='code']") %>%
        xml2::xml_text() %>%
        stringr::str_trim() -> majors

      sub_chapter_xml %>%
        xml2::xml_find_all("//div[@class='Category1']//span[@class='label']") %>%
        xml2::xml_text() %>%
        stringr::str_trim() %>%
        stringr::str_replace_all("[[:space:]]+", " ") -> majors_desc

      # sanity check
      stopifnot(length(majors) == length(majors_desc))
      names(majors) <- majors_desc

      sub_chapter_xml %>%
        xml2::xml_find_all("//div[@class='Category2']//a[@class='code']") %>%
        xml2::xml_text() %>%
        stringr::str_trim() -> leaves

      sub_chapter_xml %>%
        xml2::xml_find_all("//div[@class='Category2']//span[@class='label']") %>%
        xml2::xml_text() %>%
        stringr::str_trim() %>%
        stringr::str_replace_all("[[:space:]]+", " ") -> leaves_desc

      stopifnot(length(leaves) == length(leaves_desc))

      # someday, add the exclusion rubric to the data structure for detailed validation

      all_majors <- c(all_majors, majors)
      all_leaves <- c(all_leaves, leaves)

      print(tail(majors))
      print(tail(leaves))
    }

    # todo sort so that majors come immediately before all of their leaf children


  }
  selenium_driver$close()

  icd10_who_sub_chapters <- sub_chapters
  icd10_who_majors <- majors
  icd10_who_leaves <- leaves
  save_in_data_dir(icd10_who_sub_chapters)
  save_in_data_dir(icd10_who_majors)
  save_in_data_dir(icd10_who_leaves)


}
