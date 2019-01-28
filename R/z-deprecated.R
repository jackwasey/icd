#nocov start

d <- function(fun, ...) {
  message("'icd_' prefixed functions have been replaced with either ",
          "ICD-9 or ICD-10 specific functions, which have an ",
          "'icd9_' or 'icd10_' prefix, or lost their prefix altogether. ")
}

#' @export
`[[.icd_comorbidity_map` <-
  function(...) { d("[[.comorbidity_map"); `[[.comorbidity_map`(...) }

#' @rdname charlson
#' @template deprecated-icd3
#' @export
icd_charlson <-
  function(...) { d("charlson"); charlson(...) }

#' @rdname charlson
#' @export
icd_charlson.data.frame <-
  function(...) { d("charlson.data.frame"); charlson.data.frame(...) }

#' @rdname children
#' @template deprecated-icd3
#' @export
icd_children.character <-
  function(...) { d("children.character"); children.character(...) }

#' @rdname children
#' @export
icd_children.icd10 <-
  function(...) { d("children.icd10"); children.icd10(...) }

#' @rdname children
#' @export
icd_children.icd10cm <-
  function(...) { d("children.icd10cm"); children.icd10cm(...) }

#' @rdname children
#' @export
icd_children.icd9 <-
  function(...) { d("children.icd9"); children.icd9(...) }

#' @rdname condense
#' @template deprecated-icd3
#' @export
icd_condense.character <-
  function(...) { d("condense.character"); condense.character(...) }

#' @rdname condense
#' @export
icd_condense.icd9 <-
  function(...) { d("condense.icd9"); condense.icd9(...) }

#' @rdname decimal_to_parts
#' @export
#' @keywords internal
icd_decimal_to_parts.icd9 <-
  function(...) { d("decimal_to_parts.icd9"); decimal_to_parts.icd9(...) }

#' @rdname decimal_to_short
#' @export
#' @keywords internal
icd_decimal_to_short.default <-
  function(...) { d("decimal_to_short.default"); decimal_to_short.default(...) }

#' @rdname decimal_to_short
#' @export
#' @keywords internal
icd_decimal_to_short.icd10 <-
  function(...) { d("decimal_to_short.icd10"); decimal_to_short.icd10(...) }

#' @rdname decimal_to_short
#' @export
#' @keywords internal
icd_decimal_to_short.icd10cm <-
  function(...) { d("decimal_to_short.icd10cm"); decimal_to_short.icd10cm(...) }

#' @rdname decimal_to_short
#' @export
#' @keywords internal
icd_decimal_to_short.icd9 <-
  function(...) { d("decimal_to_short.icd9"); decimal_to_short.icd9(...) }

#' @rdname diff_comorbid
#' @template deprecated-icd3
#' @export
icd_diff_comorbid.list <-
  function(...) { d("diff_comorbid.list"); diff_comorbid.list(...) }

#' @rdname expand_range
#' @template deprecated-icd3
#' @export
icd_expand_range.character <-
  function(...) { d("expand_range.character"); expand_range.character(...) }

#' @rdname expand_range
#' @export
icd_expand_range.icd10cm <-
  function(...) { d("expand_range.icd10cm"); expand_range.icd10cm(...) }

#' @rdname expand_range
#' @export
icd_expand_range.icd9 <-
  function(...) { d("expand_range.icd9"); expand_range.icd9(...) }

#' @rdname expand_range
#' @export
icd_expand_range_major.icd9 <-
  function(...) { d("expand_range_major.icd9"); expand_range_major.icd9(...) }

#' @rdname explain_code
#' @template deprecated-icd3
#' @export
icd_explain <-
  function(...) { d("explain_code"); explain_code(...) }

#' @rdname explain_code
#' @export
icd_explain.default <-
  function(...) { d("explain_code.default"); explain_code.default(...) }

#' @rdname explain_code
#' @export
icd_explain.icd10 <-
  function(...) { d("explain_code.icd10"); explain_code.icd10(...) }

#' @rdname explain_code
#' @export
icd_explain.icd10cm <-
  function(...) { d("explain_code.icd10cm"); explain_code.icd10cm(...) }

#' @rdname explain_code
#' @export
icd_explain.icd9 <-
  function(...) { d("explain_code.icd9"); explain_code.icd9(...) }

#' @rdname explain_code
#' @export
icd_explain.icd9cm <-
  function(...) { d("explain_code.icd9cm"); explain_code.icd9cm(...) }

#' @rdname explain_code
#' @export
icd_explain.list <-
  function(...) { d("explain_code.list"); explain_code.list(...) }

#' @rdname explain_table
#' @template deprecated-icd3
#' @export
icd_explain_table.default <-
  function(...) { d("explain_table.default"); explain_table.default(...) }

#' @rdname explain_table
#' @export
icd_explain_table.icd10 <-
  function(...) { d("explain_table.icd10"); explain_table.icd10(...) }

#' @rdname explain_table
#' @export
icd_explain_table.icd10cm <-
  function(...) { d("explain_table.icd10cm"); explain_table.icd10cm(...) }

#' @rdname explain_table
#' @export
icd_explain_table.icd9 <-
  function(...) { d("explain_table.icd9"); explain_table.icd9(...) }

#' @rdname explain_table
#' @export
icd_explain_table.icd9cm <-
  function(...) { d("explain_table.icd9cm"); explain_table.icd9cm(...) }

#' @rdname get_defined
#' @template deprecated-icd3
#' @export
icd_get_defined.icd9 <-
  function(...) { d("get_defined.icd9"); get_defined.icd9(...) }

#' @rdname get_invalid
#' @template deprecated-icd3
#' @export
icd_get_invalid.comorbidity_map <-
  function(...) {
    d("get_invalid.comorbidity_map"); get_invalid.comorbidity_map(...) }

#' @rdname get_invalid
#' @export
icd_get_invalid.icd9 <-
  function(...) { d("get_invalid.icd9"); get_invalid.icd9(...) }

#' @rdname get_invalid
#' @export
icd_get_invalid.icd_comorbidity_map <-
  function(...) {
    d("icd_get_invalid.comorbidity_map"); icd_get_invalid.comorbidity_map(...) }

#' @rdname get_invalid
#' @export
icd_get_valid.icd9 <-
  function(...) { d("get_valid.icd9"); get_valid.icd9(...) }

#' @rdname guess_version
#' @export
#' @keywords internal
icd_guess_version.character <-
  function(...) { d("guess_version.character"); guess_version.character(...) }

#' @rdname guess_version
#' @export
#' @keywords internal
icd_guess_version.data.frame <-
  function(...) { d("guess_version.data.frame"); guess_version.data.frame(...) }

#' @rdname guess_version
#' @export
#' @keywords internal
icd_guess_version.factor <-
  function(...) { d("guess_version.factor"); guess_version.factor(...) }

#' @rdname guess_version
#' @export
#' @keywords internal
icd_guess_version.icd10 <-
  function(...) { d("guess_version.icd10"); guess_version.icd10(...) }

#' @rdname guess_version
#' @export
#' @keywords internal
icd_guess_version.icd9 <-
  function(...) { d("guess_version.icd9"); guess_version.icd9(...) }

#' @rdname is_billable
#' @template deprecated-icd3
#' @export
icd_is_billable.default <-
  function(...) { d("is_billable.default"); is_billable.default(...) }

#' @rdname is_billable
#' @export
icd_is_billable.icd10 <-
  function(...) { d("is_billable.icd10"); is_billable.icd10(...) }

#' @rdname is_billable
#' @export
icd_is_billable.icd10cm <-
  function(...) { d("is_billable.icd10cm"); is_billable.icd10cm(...) }

#' @rdname is_billable
#' @export
icd_is_billable.icd9 <-
  function(...) { d("is_billable.icd9"); is_billable.icd9(...) }

#' @rdname is_billable
#' @export
icd_is_billable.icd9cm <-
  function(...) { d("is_billable.icd9cm"); is_billable.icd9cm(...) }

#' @rdname is_defined
#' @template deprecated-icd3
#' @export
icd_is_defined.default <-
  function(...) { d("is_defined.default"); is_defined.default(...) }

#' @rdname is_defined
#' @export
icd_is_defined.icd10 <-
  function(...) { d("is_defined.icd10"); is_defined.icd10(...) }

#' @rdname is_defined
#' @export
icd_is_defined.icd10cm <-
  function(...) { d("is_defined.icd10cm"); is_defined.icd10cm(...) }

#' @rdname is_defined
#' @export
icd_is_defined.icd9 <-
  function(...) { d("is_defined.icd9"); is_defined.icd9(...) }

#' @rdname is_valid
#' @template deprecated-icd3
#' @export
icd_is_valid.default <-
  function(...) { d("is_valid.default"); is_valid.default(...) }

#' @rdname is_valid
#' @export
icd_is_valid.icd10 <-
  function(...) { d("is_valid.icd10"); is_valid.icd10(...) }

#' @rdname is_valid
#' @export
icd_is_valid.icd9 <-
  function(...) { d("is_valid.icd9"); is_valid.icd9(...) }

#' @rdname is_valid
#' @export
icd_is_valid.icd_comorbidity_map <-
  function(...) {
    d("icd_is_valid.comorbidity_map"); is_valid.comorbidity_map(...) }

#' @rdname is_valid
#' @export
icd_is_valid_major <-
  function(...) { d("is_valid_major"); is_valid_major(...) }

#' @rdname is_valid
#' @export
icd_is_valid_major.default <-
  function(...) { d("is_valid_major.default"); is_valid_major.default(...) }

#' @rdname is_valid
#' @export
icd_is_valid_major.icd10 <-
  function(...) { d("is_valid_major.icd10"); is_valid_major.icd10(...) }

#' @rdname is_valid
#' @export
icd_is_valid_major.icd9 <-
  function(...) { d("is_valid_major.icd9"); is_valid_major.icd9(...) }

#' @rdname short_to_decimal
#' @template deprecated-icd3
#' @export
icd_short_to_decimal.default <-
  function(...) { d("short_to_decimal.default"); short_to_decimal.default(...) }

#' @rdname short_to_decimal
#' @export
icd_short_to_decimal.icd10 <-
  function(...) { d("short_to_decimal.icd10"); short_to_decimal.icd10(...) }

#' @rdname short_to_decimal
#' @export
icd_short_to_decimal.icd10cm <-
  function(...) { d("short_to_decimal.icd10cm"); short_to_decimal.icd10cm(...) }

#' @rdname short_to_decimal
#' @template dotdotdot
#' @export
icd_short_to_decimal.icd9 <-
  function(...) { d("short_to_decimal.icd9"); short_to_decimal.icd9(...) }

#' @rdname short_to_decimal
#' @template dotdotdot
#' @export
icd_sort <-
  function(...) { d("sort_icd"); sort_icd(...) }

#' @rdname sort_icd
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_sort.default <-
  function(...) { d("sort_icd.default"); sort_icd.default(...) }

#' @rdname sort_icd
#' @template dotdotdot
#' @export
icd_sort.icd10 <-
  function(...) { d("sort_icd.icd10"); sort_icd.icd10(...) }

#' @rdname sort_icd
#' @template dotdotdot
#' @export
icd_sort.icd9 <-
  function(...) { d("sort_icd.icd9"); sort_icd.icd9(...) }

#' @rdname van_walraven
#' @template deprecated-icd3
#' @export
icd_van_walraven.data.frame <-
  function(...) { d("van_walraven.data.frame"); van_walraven.data.frame(...) }

#' @export
print.icd_comorbidity_map <-
  function(...) { d("print.comorbidity_map"); print.comorbidity_map(...) }

#' @describeIn set_icd_class Deprecated. Use as.comorbidity_map instead.
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
as.icd_comorbidity_map <-
  function(...) { d("as.comorbidity_map"); as.comorbidity_map(...) }

#' @rdname charlson_from_comorbid
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_charlson_from_comorbid <-
  function(...) { d("charlson_from_comorbid"); charlson_from_comorbid(...) }

#' @rdname children
#' @export
icd_children <-
  function(...) { d("children"); children(...) }

#' @rdname children
#' @export
icd_children_defined <-
  function(...) { d("children_defined"); children(...) }

#' @rdname children
#' @export
icd_children_defined.icd10cm <-
  function(...) { d("children_defined.icd10cm"); children_defined.icd10cm(...) }

#' @rdname comorbid
#' @template deprecated-icd3
#' @export
icd_comorbid <-
  function(...) { d("comorbid"); comorbid(...) }

#' @rdname comorbid
#' @export
icd_comorbid_ahrq <-
  function(...) { d("comorbid_ahrq"); comorbid_ahrq(...) }

#' @rdname comorbid_df_to_mat
#' @template deprecated-icd3
#' @export
icd_comorbid_df_to_mat <-
  function(...) { d("comorbid_df_to_mat"); comorbid_df_to_mat(...) }

#' @rdname comorbid
#' @export
icd_comorbid_elix <-
  function(...) { d("comorbid_elix"); comorbid_elix(...) }

#' @rdname comorbid
#' @export
icd_comorbid_hcc <-
  function(...) { d("comorbid_hcc"); comorbid_hcc(...) }

#' @rdname comorbid_df_to_mat
#' @template dotdotdot
#' @export
icd_comorbid_mat_to_df <-
  function(...) { d("comorbid_mat_to_df"); comorbid_mat_to_df(...) }

#' @rdname comorbid
#' @export
icd_comorbid_quan_deyo <-
  function(...) { d("comorbid_quan_deyo"); comorbid_quan_deyo(...) }

#' @rdname comorbid
#' @export
icd_comorbid_quan_elix <-
  function(...) { d("comorbid_quan_elix"); comorbid_quan_elix(...) }

#' @rdname condense
#' @template dotdotdot
#' @export
icd_condense <-
  function(...) { d("condense"); condense(...) }

#' @rdname count_codes
#' @template deprecated-icd3
#' @export
icd_count_codes <-
  function(...) { d("count_codes"); count_codes(...) }

#' @rdname count_codes_wide
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_count_codes_wide <-
  function(...) { d("count_codes_wide"); count_codes_wide(...) }

#' @rdname count_codes
#' @template dotdotdot
#' @export
icd_count_comorbid <-
  function(...) { d("count_comorbid"); count_comorbid(...) }

#' @rdname decimal_to_parts
#' @export
#' @keywords internal
icd_decimal_to_parts <-
  function(...) { d("decimal_to_parts"); decimal_to_parts(...) }

#' @rdname decimal_to_short
#' @export
#' @keywords internal
icd_decimal_to_short <-
  function(...) { d("decimal_to_short"); decimal_to_short(...) }

#' @rdname diff_comorbid
#' @template dotdotdot
#' @export
icd_diff_comorbid <-
  function(...) { d("diff_comorbid"); diff_comorbid(...) }

#' @rdname expand_range
#' @template dotdotdot
#' @export
icd_expand_range <-
  function(...) { d("expand_range"); expand_range(...) }

#' @rdname explain_code
#' @export
icd_explain <-
  function(...) { d("explain_code"); explain_code(...) }

#' @rdname explain_table
#' @template dotdotdot
#' @export
icd_explain_table <-
  function(...) { d("explain_table"); explain_table(...) }

#' @rdname filter_valid
#' @template deprecated-icd3
#' @export
icd_filter_invalid <-
  function(...) { d("filter_invalid"); filter_invalid(...) }

#' @rdname filter_poa
#' @template deprecated-icd3
#' @export
icd_filter_poa <-
  function(...) { d("filter_poa"); filter_poa(...) }

#' @rdname filter_poa
#' @export
icd_filter_poa_no <-
  function(...) { d("filter_poa_no"); filter_poa_no(...) }

#' @rdname filter_poa
#' @export
icd_filter_poa_not_no <-
  function(...) { d("filter_poa_not_no"); filter_poa_not_no(...) }

#' @rdname filter_poa
#' @export
icd_filter_poa_not_yes <-
  function(...) { d("filter_poa_not_yes"); filter_poa_not_yes(...) }

#' @rdname filter_poa
#' @template dotdotdot
#' @export
icd_filter_poa_yes <-
  function(...) { d("filter_poa_yes"); filter_poa_yes(...) }

#' @rdname filter_valid
#' @export
icd_filter_valid <-
  function(...) { d("filter_valid"); filter_valid(...) }

#' @rdname get_valid
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_get_valid <-
  function(...) { d("get_valid"); get_valid(...) }

#' @rdname get_billable
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_get_billable <-
  function(...) { d("get_billable"); get_billable(...) }

#' @rdname get_billable
#' @template dotdotdot
#' @export
icd_get_billable.icd9 <-
  function(...) { d("get_billable.icd9"); get_billable.icd9(...) }

#' @rdname get_billable
#' @template dotdotdot
#' @export
icd_get_billable.icd9cm <-
  function(...) { d("get_billable.icd9cm"); get_billable.icd9cm(...) }

#' @rdname get_billable
#' @template dotdotdot
#' @export
icd_get_billable.icd10 <-
  function(...) { d("get_billable.icd10"); get_billable.icd10(...) }

#' @rdname get_billable
#' @template dotdotdot
#' @export
icd_get_billable.icd10cm <-
  function(...) { d("get_billable.icd10cm"); get_billable.icd10cm(...) }

#' @rdname get_defined
#' @template dotdotdot
#' @export
icd_get_defined <-
  function(...) { d("get_defined"); get_defined(...) }

#' @rdname guess_version
#' @export
#' @keywords internal
icd_guess_version <-
  function(...) { d("guess_version"); guess_version(...) }

#' @rdname is_billable
#' @template dotdotdot
#' @export
icd_is_billable <-
  function(...) { d("is_billable"); is_billable(...) }

#' @rdname is_defined
#' @template dotdotdot
#' @export
icd_is_defined <-
  function(...) { d("is_defined"); is_defined(...) }

#' @rdname is_valid
#' @template dotdotdot
#' @export
icd_is_valid <-
  function(...) { d("is_valid"); is_valid(...) }

#' @rdname long_to_wide
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_long_to_wide <-
  function(...) { d("long_to_wide"); long_to_wide(...) }

#' @rdname poa_choices
#' @template deprecated-icd3
#' @export
icd_poa_choices <- poa_choices

#' @rdname short_to_decimal
#' @template dotdotdot
#' @export
icd_short_to_decimal <-
  function(...) { d("short_to_decimal"); short_to_decimal(...) }

#' @rdname van_walraven
#' @export
icd_van_walraven <-
  function(...) { d("van_walraven"); van_walraven(...) }

#' @rdname van_walraven
#' @export
icd_van_walraven_from_comorbid <-
  function(...) {
    d("van_walraven_from_comorbid"); van_walraven_from_comorbid(...) }

#' @rdname wide_to_long
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
icd_wide_to_long <-
  function(...) { d("wide_to_long"); wide_to_long(...) }

#' @rdname is.icd9
#' @template deprecated-icd3
#' @template dotdotdot
#' @export
is.icd_comorbidity_map <-
  function(...) { d("is.comorbidity_map"); is.comorbidity_map(...) }

#nocov end
