args = commandArgs(trailingOnly = TRUE)
if (tolower(args[[1]]) == "true") {

  options(warn = -1)

  line_exclusions = list(
    'inst/include/icd_RcppExports.h',
    'R/children_alt.R',
    'R/comorbid_alt.R',
    'R/parse-icd10cm-xml.R',
    'R/ranges_alt.R',
    'R/sort_alt.R',
    'src/convert_alt.cpp',
    'src/manip_alt.cpp',
    'src/ranges_alt.cpp',
    'R/fuzz.R')

  if (args[[2]] == "coveralls")
    covr::coveralls(quiet = TRUE, line_exclusions = line_exclusions)
  else if (args[[2]] == "codecov")
    covr::codecov(quiet = TRUE, line_exclusions = line_exclusions)
  else
    stop("Must use codecov or coveralls.")

  covr::package_coverage(type = "vignette", quiet = TRUE, line_exclusions = line_exclusions)
}
