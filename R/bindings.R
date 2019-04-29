# Set up an environemnt to cache ICD data
.icd_data_env <- new.env(parent = emptyenv())

.data_names_pkg <- c(
  "icd9cm2014_leaf",
  "icd10cm2016", # included (but will migrate to 2019 once all is on CRAN)
  "icd10cm2019" # included in package data, being the latest version
)

.data_names_cache <- c(
  # WHO
  "icd10who2016",
  "icd10who2008fr",
  # FR
  "icd10fr2019",
  # BE
  "icd10be2014",
  "icd10be2014_pc",
  "icd10be2017",
  "icd10be2017_pc",
  # ICD-9-CM leaf descriptions
  paste0("icd9cm", 2005:2013, "_leaf"),
  # RTF parsing with majors "three-digit" codes and other non-leaf nodes
  paste0("icd9cm", 2005:2014),
  # ICD-10-CM PCS
  paste0("icd10cm", 2014:2019, "_pc"),
  # ICD-10-CM DX
  "icd10cm2014",
  "icd10cm2015",
  "icd10cm2017",
  "icd10cm2018"
)

.data_names <- c(
  .data_names_pkg,
  .data_names_cache
)

#' Localised synonym for \code{\link{get_icd10fr2019}}, with French column names
#' @seealso \code{\link{get_icd10fr2019}}
#' @export
get_cim10fr2019 <- function() {
  if (exists("cim10fr2019", envir = .icd_data_env)) {
    return(get("cim10fr2019", envir = .icd_data_env))
  }
  cim10fr2019 <- get_icd10fr2019()
  names(cim10fr2019) <- c(
    "code",
    "desc_courte",
    "desc_longue",
    "majeure",
    "trois_chiffres"
  )
  rownames(cim10fr2019) <- NULL
  assign("cim10fr2019",
    value = cim10fr2019,
    envir = .icd_data_env
  )
  cim10fr2019
}
