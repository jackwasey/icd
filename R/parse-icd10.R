
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


#' scrape WHO web site for ICD-10 codes
#'
#' javascript only (at least in recent years), so can't just get the HTML.
#' Thanks guys.
#'
#' also requires phatomjs (which is Mac/Win only?) to render the javascript
#' @import RSelenium xml2
#' @keywords internal
scrape_icd10_who <- function(sleep_secs = 3) {
  requireNamespace("RSelenium")
  requireNamespace("magrittr")
  requireNamespace("xml2")
  requireNamespace("rvest")
  url <- "http://apps.who.int/classifications/icd10/browse/2016/en"

  if (Sys.info()[["sysname"]] == "Windows")
    phantomjs <- RSelenium::phantom(pjs_cmd = "tools/phantomjs.exe")
  else
    phantomjs <- RSelenium::phantom()
  remDr <- RSelenium::remoteDriver(browserName = "phantomjs")

  remDr$open()

  on.exit({
    message("exiting scrape")
    remDr$close()
    phantomjs$stop()
    message("closed and stopped")
  })

  remDr$navigate(url)

  # now what have we got?
  main_index_src <- remDr$getPageSource()
  main_index <- xml2::read_html(main_index_src[[1]])
  writeLines(main_index_src[[1]], "temp.html")

  # show all a tags:
  print(xml2::xml_find_all(main_index, "//a"))
  # get all those a nodes: (?just at one level)
  a_nodes <- rvest::html_nodes(main_index, "a")

  # find just the anchors with the given class
  a_nodes <- rvest::html_nodes(a_nodes, xpath = "//a[@class='ygtvlabel  ']") %>%
    # and clean up:
    rvest::html_text() %>%
    trim() %>%
    stringr::str_replace("^[^ ]+ ", "")

  # but we already knew the chapters, we need to click the links...

  # same using selenium/phantom:
  chapterVII <- remDr$findElement(using = 'xpath', "//a[@data-id='VII']")

  # click a link:
  # (press enter) chapterVII$sendKeysToElement(list("\uE007"))
  print(remDr$getCurrentUrl())

  chapterVII$clickElement()

  # did URL change?
  print(remDr$getCurrentUrl())

  # and see what we have now:
  chapterVII_clicked <- remDr$getPageSource()[[1]]
  writeLines(chapterVII_clicked[[1]], "chapter7.html")

  # yes, we now have things like:
  # <li class="Blocklist1">
  #   <a href="#/H15-H22" title="Disorders of sclera, cornea, iris and ciliary body" class="code">H15-H22</a>
  #   <span class="label">Disorders of
  # sclera, cornea, iris and ciliary body</span>
  #   </li>


  #Hfifteen <- remDr$findElements(using = "xpath", "//li[@class='Blocklist1']")
  #print(Hfifteen[[1]]$getElementText()) # now we have a subshapter
  #  "H00-H06\nDisorders of eyelid, lacrimal system and orbit"





  ## now do it looping through everything:

  # get the main chapters index

  # for each chapter roman numerals up to XXII
  #    generate URL from chapter name, e.g.
  #     http://apps.who.int/classifications/icd10/browse/2016/en#/VII
  #     scrape sub-chapter names and ranges: e.g. H30-H36 Disorders of choroid and retina

  chapter_urls <- paste0("http://apps.who.int/classifications/icd10/browse/2016/en#/",
                         as.roman(1:21))

  sub_chapters <- list()

  for (chapter_url in chapter_urls) {
    message(chapter_url)
    # Sys.sleep(sleep_secs) # this seems to hang my Win 7 sporadically
    remDr$navigate(chapter_url)
    #chapter_html <- remDr$getPageSource()
    # chapter_xml <- xml2::read_html(chapter_html[[1]])
    sub_chapters_xml <- remDr$findElements(using = "xpath", "//li[@class='Blocklist1']")
    message("got sub_chapters")
    sub_chapters <- lapply(sub_chapters_xml, function(x)
      strsplit(x$getElementText()[[1]], "\\n")[[1]][1] %>% strsplit("-")
    )
    names(sub_chapters) <- lapply(sub_chapters_xml, function(x)
      strsplit(x$getElementText()[[1]], "\\n")[[1]][2]
    )
    message("starting sub loop")
    for (sub_chapter_xml in sub_chapters_xml) {
      message("sub_chapter loop")
      pair <- strsplit(sub_chapter_xml$getElementText()[[1]], "\\n")[[1]]

      print(pair)
      sub_chapter_range <- pair[1]
      sub_chapter_title <- pair[2]

    }

  }

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




}
