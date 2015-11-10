
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


#' scrape WHO web site
#'
#' javascript only (at least in recent years)
#'
#' also requires phatomjs (which is Mac/Win only?) to render the javascript
#' @import RSelenium
#' @keywords internal
scrape_icd10_who <- function() {
  url <- "http://apps.who.int/classifications/icd10/browse/2016/en"
  pJS <- phantom(pjs_cmd = "tools/phantomjs.exe")
  remDr <- remoteDriver(browserName = "phantomjs")
  remDr$open()
  remDr$navigate(url)

  # now what have we got?
  main_index_src <- remDr$getPageSource()
  main_index <- read_html(main_index_src[[1]])
  writeLines(main_index_src[[1]], "temp.html")

  # show all a tags:
  print(xml2::xml_find_all(main_index, "//a"))
  # get all those a nodes: (?just at one level)
  #a_nodes <- html_nodes(main_index, "a")

  # find just the anchors with the given class
  a_nodes <- html_nodes(a_nodes, xpath = "//a[@class='ygtvlabel  ']") %>%
    # and clean up:
    html_text() %>%
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


  remDr$close()
  pJS@stop()


}
