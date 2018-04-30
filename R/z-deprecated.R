# git diff -U0 HEAD~4 -- NAMESPACE | grep icd_ | sed 's/.*(\(.*\)\(icd_\)\(.*\)).*/\1\2\3 <- function(...) { very_soft_deprecate("\1\3"); \1\3(...) }/' | tr , .

#nocov start

very_soft_deprecate <- function(fun, ...) {}

#' @export
`[[.icd_comorbidity_map` <- function(...) { very_soft_deprecate("[[.comorbidity_map"); `[[.comorbidity_map`(...) }

#' @rdname charlson
#' @template deprecated-icd3
#' @export
icd_charlson <- function(...) { very_soft_deprecate("charlson"); charlson(...) }

#' @rdname charlson
#' @template deprecated-icd3
#' @export
icd_charlson.data.frame <- function(...) { very_soft_deprecate("charlson.data.frame"); charlson.data.frame(...) }

#' @rdname children
#' @template deprecated-icd3
#' @export
icd_children.character <- function(...) { very_soft_deprecate("children.character"); children.character(...) }

#' @rdname children
#' @template deprecated-icd3
#' @export
icd_children.icd10 <- function(...) { very_soft_deprecate("children.icd10"); children.icd10(...) }

#' @rdname children
#' @template deprecated-icd3
#' @export
icd_children.icd10cm <- function(...) { very_soft_deprecate("children.icd10cm"); children.icd10cm(...) }

#' @rdname children
#' @template deprecated-icd3
#' @export
icd_children.icd9 <- function(...) { very_soft_deprecate("children.icd9"); children.icd9(...) }

#' @rdname condense
#' @template deprecated-icd3
#' @export
icd_condense.character <- function(...) { very_soft_deprecate("condense.character"); condense.character(...) }

#' @rdname condense
#' @template deprecated-icd3
#' @export
icd_condense.icd9 <- function(...) { very_soft_deprecate("condense.icd9"); condense.icd9(...) }

#' @rdname decimal_to_parts
#' @export
#' @keywords internal
icd_decimal_to_parts.icd9 <- function(...) { very_soft_deprecate("decimal_to_parts.icd9"); decimal_to_parts.icd9(...) }

#' @rdname decimal_to_short
#' @export
#' @keywords internal
icd_decimal_to_short.default <- function(...) { very_soft_deprecate("decimal_to_short.default"); decimal_to_short.default(...) }

#' @rdname decimal_to_short
#' @export
#' @keywords internal
icd_decimal_to_short.icd10 <- function(...) { very_soft_deprecate("decimal_to_short.icd10"); decimal_to_short.icd10(...) }

#' @rdname decimal_to_short
#' @export
#' @keywords internal
icd_decimal_to_short.icd10cm <- function(...) { very_soft_deprecate("decimal_to_short.icd10cm"); decimal_to_short.icd10cm(...) }

#' @rdname decimal_to_short
#' @export
#' @keywords internal
icd_decimal_to_short.icd9 <- function(...) { very_soft_deprecate("decimal_to_short.icd9"); decimal_to_short.icd9(...) }

#' @rdname diff_comorbid
#' @template deprecated-icd3
#' @export
icd_diff_comorbid.list <- function(...) { very_soft_deprecate("diff_comorbid.list"); diff_comorbid.list(...) }

#' @rdname expand_range
#' @template deprecated-icd3
#' @export
icd_expand_range.character <- function(...) { very_soft_deprecate("expand_range.character"); expand_range.character(...) }

#' @rdname expand_range
#' @template deprecated-icd3
#' @export
icd_expand_range.icd10cm <- function(...) { very_soft_deprecate("expand_range.icd10cm"); expand_range.icd10cm(...) }

#' @rdname expand_range
#' @template deprecated-icd3
#' @export
icd_expand_range.icd9 <- function(...) { very_soft_deprecate("expand_range.icd9"); expand_range.icd9(...) }

#' @rdname expand_range
#' @template deprecated-icd3
#' @export
icd_expand_range_major.icd9 <- function(...) { very_soft_deprecate("expand_range_major.icd9"); expand_range_major.icd9(...) }

#' @rdname explain
#' @template deprecated-icd3
#' @export
icd_explain.default <- function(...) { very_soft_deprecate("explain.default"); explain.default(...) }

#' @rdname explain
#' @template deprecated-icd3
#' @export
icd_explain.icd10 <- function(...) { very_soft_deprecate("explain.icd10"); explain.icd10(...) }

#' @rdname explain
#' @template deprecated-icd3
#' @export
icd_explain.icd10cm <- function(...) { very_soft_deprecate("explain.icd10cm"); explain.icd10cm(...) }

#' @rdname explain
#' @template deprecated-icd3
#' @export
icd_explain.icd9 <- function(...) { very_soft_deprecate("explain.icd9"); explain.icd9(...) }

#' @rdname explain
#' @template deprecated-icd3
#' @export
icd_explain.icd9cm <- function(...) { very_soft_deprecate("explain.icd9cm"); explain.icd9cm(...) }

#' @rdname explain
#' @template deprecated-icd3
#' @export
icd_explain.list <- function(...) { very_soft_deprecate("explain.list"); explain.list(...) }

#' @rdname explain
#' @template deprecated-icd3
#' @export
icd_explain_table.default <- function(...) { very_soft_deprecate("explain_table.default"); explain_table.default(...) }

#' @rdname explain_table
#' @template deprecated-icd3
#' @export
icd_explain_table.icd10 <- function(...) { very_soft_deprecate("explain_table.icd10"); explain_table.icd10(...) }

#' @rdname explain_table
#' @template deprecated-icd3
#' @export
icd_explain_table.icd10cm <- function(...) { very_soft_deprecate("explain_table.icd10cm"); explain_table.icd10cm(...) }

#' @rdname explain_table
#' @template deprecated-icd3
#' @export
icd_explain_table.icd9 <- function(...) { very_soft_deprecate("explain_table.icd9"); explain_table.icd9(...) }

#' @rdname explain_table
#' @template deprecated-icd3
#' @export
icd_explain_table.icd9cm <- function(...) { very_soft_deprecate("explain_table.icd9cm"); explain_table.icd9cm(...) }

#' @rdname get_defined
#' @template deprecated-icd3
#' @export
icd_get_defined.icd9 <- function(...) { very_soft_deprecate("get_defined.icd9"); get_defined.icd9(...) }

#' @rdname get_invalid
#' @template deprecated-icd3
#' @export
icd_get_invalid.comorbidity_map <- function(...) { very_soft_deprecate("get_invalid.comorbidity_map"); get_invalid.comorbidity_map(...) }

#' @rdname get_invalid
#' @template deprecated-icd3
#' @export
icd_get_invalid.icd9 <- function(...) { very_soft_deprecate("get_invalid.icd9"); get_invalid.icd9(...) }

#' @rdname get_invalid
#' @template deprecated-icd3
#' @export
icd_get_invalid.icd_comorbidity_map <- function(...) { very_soft_deprecate("icd_get_invalid.comorbidity_map"); icd_get_invalid.comorbidity_map(...) }

#' @rdname get_invalid
#' @template deprecated-icd3
#' @export
icd_get_valid.icd9 <- function(...) { very_soft_deprecate("get_valid.icd9"); get_valid.icd9(...) }

#' @rdname guess_version
#' @export
#' @keywords internal
icd_guess_version.character <- function(...) { very_soft_deprecate("guess_version.character"); guess_version.character(...) }

#' @rdname guess_version
#' @export
#' @keywords internal
icd_guess_version.data.frame <- function(...) { very_soft_deprecate("guess_version.data.frame"); guess_version.data.frame(...) }

#' @rdname guess_version
#' @export
#' @keywords internal
icd_guess_version.factor <- function(...) { very_soft_deprecate("guess_version.factor"); guess_version.factor(...) }

#' @rdname guess_version
#' @export
#' @keywords internal
icd_guess_version.icd10 <- function(...) { very_soft_deprecate("guess_version.icd10"); guess_version.icd10(...) }

#' @rdname guess_version
#' @export
#' @keywords internal
icd_guess_version.icd9 <- function(...) { very_soft_deprecate("guess_version.icd9"); guess_version.icd9(...) }

#' @rdname is_billable
#' @template deprecated-icd3
#' @export
icd_is_billable.default <- function(...) { very_soft_deprecate("is_billable.default"); is_billable.default(...) }

#' @rdname is_billable
#' @template deprecated-icd3
#' @export
icd_is_billable.icd10 <- function(...) { very_soft_deprecate("is_billable.icd10"); is_billable.icd10(...) }

#' @rdname is_billable
#' @template deprecated-icd3
#' @export
icd_is_billable.icd10cm <- function(...) { very_soft_deprecate("is_billable.icd10cm"); is_billable.icd10cm(...) }

#' @rdname is_billable
#' @template deprecated-icd3
#' @export is_billable
icd_is_billable.icd9 <- function(...) { very_soft_deprecate("is_billable.icd9"); is_billable.icd9(...) }

#' @rdname is_billable
#' @template deprecated-icd3
#' @export
icd_is_billable.icd9cm <- function(...) { very_soft_deprecate("is_billable.icd9cm"); is_billable.icd9cm(...) }

#' @rdname is_defined
#' @template deprecated-icd3
#' @export
icd_is_defined.default <- function(...) { very_soft_deprecate("is_defined.default"); is_defined.default(...) }

#' @rdname is_defined
#' @template deprecated-icd3
#' @export
icd_is_defined.icd10 <- function(...) { very_soft_deprecate("is_defined.icd10"); is_defined.icd10(...) }

#' @rdname is_defined
#' @template deprecated-icd3
#' @export
icd_is_defined.icd10cm <- function(...) { very_soft_deprecate("is_defined.icd10cm"); is_defined.icd10cm(...) }

#' @rdname is_defined
#' @template deprecated-icd3
#' @export
icd_is_defined.icd9 <- function(...) { very_soft_deprecate("is_defined.icd9"); is_defined.icd9(...) }

#' @rdname is_valid
#' @template deprecated-icd3
#' @export
icd_is_valid.default <- function(...) { very_soft_deprecate("is_valid.default"); is_valid.default(...) }

#' @rdname is_valid
#' @template deprecated-icd3
#' @export
icd_is_valid.icd10 <- function(...) { very_soft_deprecate("is_valid.icd10"); is_valid.icd10(...) }

#' @rdname is_valid
#' @template deprecated-icd3
#' @export
icd_is_valid.icd9 <- function(...) { very_soft_deprecate("is_valid.icd9"); is_valid.icd9(...) }

#' @rdname is_valid
#' @template deprecated-icd3
#' @export
icd_is_valid.icd_comorbidity_map <- function(...) { very_soft_deprecate("icd_is_valid.comorbidity_map"); is_valid.comorbidity_map(...) }

#' @rdname is_valid
#' @template deprecated-icd3
#' @export
icd_is_valid_major <- function(...) { very_soft_deprecate("is_valid_major"); is_valid_major(...) }

#' @rdname is_valid
#' @template deprecated-icd3
#' @export
icd_is_valid_major.default <- function(...) { very_soft_deprecate("is_valid_major.default"); is_valid_major.default(...) }

#' @rdname is_valid
#' @template deprecated-icd3
#' @export
icd_is_valid_major.icd10 <- function(...) { very_soft_deprecate("is_valid_major.icd10"); is_valid_major.icd10(...) }

#' @rdname is_valid
#' @template deprecated-icd3
#' @export
icd_is_valid_major.icd9 <- function(...) { very_soft_deprecate("is_valid_major.icd9"); is_valid_major.icd9(...) }

#' @rdname short_to_decimal
#' @template deprecated-icd3
#' @export
icd_short_to_decimal.default <- function(...) { very_soft_deprecate("short_to_decimal.default"); short_to_decimal.default(...) }

#' @rdname short_to_decimal
#' @template deprecated-icd3
#' @export
icd_short_to_decimal.icd10 <- function(...) { very_soft_deprecate("short_to_decimal.icd10"); short_to_decimal.icd10(...) }

#' @rdname short_to_decimal
#' @template deprecated-icd3
#' @export
icd_short_to_decimal.icd10cm <- function(...) { very_soft_deprecate("short_to_decimal.icd10cm"); short_to_decimal.icd10cm(...) }

#' @rdname short_to_decimal
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_short_to_decimal.icd9 <- function(...) { very_soft_deprecate("short_to_decimal.icd9"); short_to_decimal.icd9(...) }

#' @rdname short_to_decimal
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_sort <- function(...) { very_soft_deprecate("sort_icd"); sort_icd(...) }

#' @rdname sort_icd
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_sort.default <- function(...) { very_soft_deprecate("sort_icd.default"); sort_icd.default(...) }

#' @rdname sort_icd
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_sort.icd10 <- function(...) { very_soft_deprecate("sort_icd.icd10"); sort_icd.icd10(...) }

#' @rdname sort_icd
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_sort.icd9 <- function(...) { very_soft_deprecate("sort_icd.icd9"); sort_icd.icd9(...) }

#' @rdname van_walraven
#' @template deprecated-icd3
#' @export
icd_van_walraven.data.frame <- function(...) { very_soft_deprecate("van_walraven.data.frame"); van_walraven.data.frame(...) }

#' @export
print.icd_comorbidity_map <- function(...) { very_soft_deprecate("print.comorbidity_map"); print.comorbidity_map(...) }

#' @rdname set_icd_class
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
as.icd_comorbidity_map <- function(...) { very_soft_deprecate("as.comorbidity_map"); as.comorbidity_map(...) }

#' @rdname charlson_from_comorbid
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_charlson_from_comorbid <- function(...) { very_soft_deprecate("charlson_from_comorbid"); charlson_from_comorbid(...) }

#' @rdname children
#' @template deprecated-icd3
#' @export
icd_children <- function(...) { very_soft_deprecate("children"); children(...) }

#' @rdname children
#' @template deprecated-icd3
#' @export
icd_children_defined <- function(...) { very_soft_deprecate("children_defined"); children(...) }

#' @rdname children
#' @template deprecated-icd3
#' @export
icd_children_defined.icd10cm <- function(...) { very_soft_deprecate("children_defined.icd10cm"); children_defined.icd10cm(...) }

#' @rdname comorbid
#' @template deprecated-icd3
#' @export
icd_comorbid <- function(...) { very_soft_deprecate("comorbid"); comorbid(...) }

#' @rdname comorbid
#' @template deprecated-icd3
#' @export
icd_comorbid_ahrq <- function(...) { very_soft_deprecate("comorbid_ahrq"); comorbid_ahrq(...) }

#' @rdname comorbid_df_to_mat
#' @template deprecated-icd3
#' @export
icd_comorbid_df_to_mat <- function(...) { very_soft_deprecate("comorbid_df_to_mat"); comorbid_df_to_mat(...) }

#' @rdname comorbid
#' @template deprecated-icd3
#' @export
icd_comorbid_elix <- function(...) { very_soft_deprecate("comorbid_elix"); comorbid_elix(...) }

#' @rdname comorbid
#' @template deprecated-icd3
#' @export
icd_comorbid_hcc <- function(...) { very_soft_deprecate("comorbid_hcc"); comorbid_hcc(...) }

#' @rdname comorbid_df_to_mat
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_comorbid_mat_to_df <- function(...) { very_soft_deprecate("comorbid_mat_to_df"); comorbid_mat_to_df(...) }

#' @rdname comorbid
#' @template deprecated-icd3
#' @export
icd_comorbid_quan_deyo <- function(...) { very_soft_deprecate("comorbid_quan_deyo"); comorbid_quan_deyo(...) }

#' @rdname comorbid
#' @template deprecated-icd3
#' @export
icd_comorbid_quan_elix <- function(...) { very_soft_deprecate("comorbid_quan_elix"); comorbid_quan_elix(...) }

#' @rdname condense
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_condense <- function(...) { very_soft_deprecate("condense"); condense(...) }

#' @rdname count_codes
#' @template deprecated-icd3
#' @export
icd_count_codes <- function(...) { very_soft_deprecate("count_codes"); count_codes(...) }

#' @rdname count_codes_wide
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_count_codes_wide <- function(...) { very_soft_deprecate("count_codes_wide"); count_codes_wide(...) }

#' @rdname count_codes
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_count_comorbid <- function(...) { very_soft_deprecate("count_comorbid"); count_comorbid(...) }

#' @rdname decimal_to_parts
#' @export
#' @keywords internal
icd_decimal_to_parts <- function(...) { very_soft_deprecate("decimal_to_parts"); decimal_to_parts(...) }

#' @rdname decimal_to_short
#' @export
#' @keywords internal
icd_decimal_to_short <- function(...) { very_soft_deprecate("decimal_to_short"); decimal_to_short(...) }

#' @rdname diff_comorbid
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_diff_comorbid <- function(...) { very_soft_deprecate("diff_comorbid"); diff_comorbid(...) }

#' @rdname expand_range
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_expand_range <- function(...) { very_soft_deprecate("expand_range"); expand_range(...) }

#' @rdname explain
#' @template deprecated-icd3
#' @export
icd_explain <- function(...) { very_soft_deprecate("explain"); explain(...) }

#' @rdname explain_table
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_explain_table <- function(...) { very_soft_deprecate("explain_table"); explain_table(...) }

#' @rdname filter_valid
#' @template deprecated-icd3
#' @export
icd_filter_invalid <- function(...) { very_soft_deprecate("filter_invalid"); filter_invalid(...) }

#' @rdname filter_poa
#' @template deprecated-icd3
#' @export
icd_filter_poa <- function(...) { very_soft_deprecate("filter_poa"); filter_poa(...) }

#' @rdname filter_poa
#' @template deprecated-icd3
#' @export
icd_filter_poa_no <- function(...) { very_soft_deprecate("filter_poa_no"); filter_poa_no(...) }

#' @rdname filter_poa
#' @template deprecated-icd3
#' @export
icd_filter_poa_not_no <- function(...) { very_soft_deprecate("filter_poa_not_no"); filter_poa_not_no(...) }

#' @rdname filter_poa
#' @template deprecated-icd3
#' @export
icd_filter_poa_not_yes <- function(...) { very_soft_deprecate("filter_poa_not_yes"); filter_poa_not_yes(...) }

#' @rdname filter_poa
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_filter_poa_yes <- function(...) { very_soft_deprecate("filter_poa_yes"); filter_poa_yes(...) }

#' @rdname filter_valid
#' @template deprecated-icd3
#' @export
icd_filter_valid <- function(...) { very_soft_deprecate("filter_valid"); filter_valid(...) }

#' @rdname get_valid
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_get_valid <- function(...) { very_soft_deprecate("get_valid"); get_valid(...) }

#' @rdname get_billable
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_get_billable <- function(...) { very_soft_deprecate("get_billable"); get_billable(...) }

#' @rdname get_billable
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_get_billable.icd9 <- function(...) { very_soft_deprecate("get_billable.icd9"); get_billable.icd9(...) }

#' @rdname get_billable
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_get_billable.icd9cm <- function(...) { very_soft_deprecate("get_billable.icd9cm"); get_billable.icd9cm(...) }

#' @rdname get_billable
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_get_billable.icd10 <- function(...) { very_soft_deprecate("get_billable.icd10"); get_billable.icd10(...) }

#' @rdname get_billable
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_get_billable.icd10cm <- function(...) { very_soft_deprecate("get_billable.icd10cm"); get_billable.icd10cm(...) }

#' @rdname get_defined
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_get_defined <- function(...) { very_soft_deprecate("get_defined"); get_defined(...) }

#' @rdname guess_version
#' @export
#' @keywords internal
icd_guess_version <- function(...) { very_soft_deprecate("guess_version"); guess_version(...) }

#' @rdname is_billable
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_is_billable <- function(...) { very_soft_deprecate("is_billable"); is_billable(...) }

#' @rdname is_defined
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_is_defined <- function(...) { very_soft_deprecate("is_defined"); is_defined(...) }

#' @rdname is_valid
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_is_valid <- function(...) { very_soft_deprecate("is_valid"); is_valid(...) }

#' @rdname long_to_wide
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_long_to_wide <- function(...) { very_soft_deprecate("long_to_wide"); long_to_wide(...) }

#' @rdname poa_choices
#' @template deprecated-icd3
#' @export
icd_poa_choices <- poa_choices

#' @rdname short_to_decimal
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_short_to_decimal <- function(...) { very_soft_deprecate("short_to_decimal"); short_to_decimal(...) }

#' @rdname van_walraven
#' @template deprecated-icd3
#' @export
icd_van_walraven <- function(...) { very_soft_deprecate("van_walraven"); van_walraven(...) }

#' @rdname van_walraven
#' @template deprecated-icd3
#' @export
icd_van_walraven_from_comorbid <- function(...) { very_soft_deprecate("van_walraven_from_comorbid"); van_walraven_from_comorbid(...) }

#' @rdname wide_to_long
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_wide_to_long <- function(...) { very_soft_deprecate("wide_to_long"); wide_to_long(...) }

#' @rdname is.icd9
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
is.icd_comorbidity_map <- function(...) { very_soft_deprecate("is.comorbidity_map"); is.comorbidity_map(...) }

#nocov end
