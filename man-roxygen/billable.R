#' @param leaf single logical value, whether to limit return codes also by
#'   whether they are billable, i.e. leaf nodes. This is really only designed
#'   for use with ICD-9-CM, ICD-10-CM etc, since the WHO versions are not
#'   designed for billing, but for public health and death reporting.
#' @param billable single logical value, identical to 'leaf'. Leaf is preferred
#'   as most adaptations of WHO ICD codes are not oriented around money.
