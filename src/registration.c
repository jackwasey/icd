#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _icd_comorbidMatMul(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _icd_debug_parallel();
extern SEXP _icd_debug_parallel_env();
extern SEXP _icd_factor_fast(SEXP);
extern SEXP _icd_factorNoSort(SEXP, SEXP);
extern SEXP _icd_fastIntToString_alt_Std(SEXP);
extern SEXP _icd_fastIntToStringRcpp(SEXP);
extern SEXP _icd_getOmpCores();
extern SEXP _icd_getOmpMaxThreads();
extern SEXP _icd_getOmpThreads();
extern SEXP _icd_guessShortCompleteCpp(SEXP, SEXP, SEXP, SEXP);
extern SEXP _icd_guessShortPlusFactorCpp(SEXP, SEXP);
extern SEXP _icd_icd10cmChildrenDefined(SEXP, SEXP, SEXP);
extern SEXP _icd_icd10ComorbidParentSearchCpp(SEXP, SEXP, SEXP, SEXP);
extern SEXP _icd_icd10DecimalToPartsCpp(SEXP, SEXP);
extern SEXP _icd_icd10ShortToPartsCpp(SEXP, SEXP);
extern SEXP _icd_icd9_is_e_cpp(SEXP);
extern SEXP _icd_icd9_is_n_cpp(SEXP);
extern SEXP _icd_icd9_is_v_cpp(SEXP);
extern SEXP _icd_icd9AddLeadingZeroes(SEXP, SEXP);
extern SEXP _icd_icd9AddLeadingZeroes_alt_Direct(SEXP, SEXP);
extern SEXP _icd_icd9AddLeadingZeroes_alt_ShortSingle(SEXP);
extern SEXP _icd_icd9AddLeadingZeroesMajor(SEXP);
extern SEXP _icd_icd9AddLeadingZeroesMajorSingle(SEXP);
extern SEXP _icd_icd9AddLeadingZeroesMajorSingleStd(SEXP);
extern SEXP _icd_icd9AppendMinors(SEXP, SEXP, SEXP);
extern SEXP _icd_icd9Children_alt_ShortNoNaUnordered(SEXP, SEXP);
extern SEXP _icd_icd9ChildrenCpp(SEXP, SEXP, SEXP, SEXP);
extern SEXP _icd_icd9ChildrenDecimalCpp(SEXP, SEXP, SEXP);
extern SEXP _icd_icd9ChildrenShort(SEXP, SEXP, SEXP);
extern SEXP _icd_icd9ChildrenShort_alt_11(SEXP, SEXP);
extern SEXP _icd_icd9ChildrenShort_alt_Std(SEXP, SEXP);
extern SEXP _icd_icd9ChildrenShortUnordered(SEXP, SEXP, SEXP);
extern SEXP _icd_icd9Comorbid_alt_Taskloop(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _icd_icd9Comorbid_alt_Taskloop2(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _icd_icd9ComorbidShortCpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _icd_icd9DecimalToPartsCpp(SEXP, SEXP);
extern SEXP _icd_icd9DecimalToShort(SEXP);
extern SEXP _icd_icd9ExpandMinor(SEXP, SEXP);
extern SEXP _icd_icd9ExpandMinor_alt_Std(SEXP, SEXP);
extern SEXP _icd_icd9GetMajor(SEXP, SEXP);
extern SEXP _icd_icd9LongToWideCpp(SEXP, SEXP, SEXP, SEXP);
extern SEXP _icd_icd9MajMinToCode(SEXP, SEXP, SEXP);
extern SEXP _icd_icd9MajMinToCode_alt_Old(SEXP, SEXP, SEXP);
extern SEXP _icd_icd9MajMinToCode_alt_PrePadded(SEXP, SEXP, SEXP);
extern SEXP _icd_icd9MajMinToCode_alt_Std(SEXP, SEXP, SEXP);
extern SEXP _icd_icd9MajMinToDecimal(SEXP, SEXP);
extern SEXP _icd_icd9MajMinToParts(SEXP, SEXP);
extern SEXP _icd_icd9MajMinToShort(SEXP, SEXP);
extern SEXP _icd_icd9MajMinToShort_alt_Std(SEXP, SEXP);
extern SEXP _icd_icd9MajMinToShortSingle_alt_Std(SEXP, SEXP);
extern SEXP _icd_icd9OrderCpp(SEXP);
extern SEXP _icd_icd9PartsToDecimal(SEXP);
extern SEXP _icd_icd9PartsToShort(SEXP);
extern SEXP _icd_icd9RandomShort(SEXP);
extern SEXP _icd_icd9RandomShortE(SEXP);
extern SEXP _icd_icd9RandomShortN(SEXP);
extern SEXP _icd_icd9RandomShortV(SEXP);
extern SEXP _icd_icd9ShortToDecimal(SEXP);
extern SEXP _icd_icd9ShortToPartsCpp(SEXP, SEXP);
extern SEXP _icd_icd9SortCpp(SEXP);
extern SEXP _icd_lookupComorbid_alt_ByChunkForTaskloop(SEXP, SEXP, SEXP);
extern SEXP _icd_lookupComorbidByChunkFor(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _icd_randomMajorCpp(SEXP);
extern SEXP _icd_RcppExport_registerCCallable();
extern SEXP _icd_setDecimalDiag(SEXP, SEXP);
extern SEXP _icd_setShortDiag(SEXP, SEXP);
extern SEXP _icd_simplifyMapLexicographic(SEXP, SEXP);
extern SEXP _icd_strimCpp(SEXP);
extern SEXP _icd_trimCpp(SEXP);
extern SEXP _icd_trimLeftCpp(SEXP);
extern SEXP _icd_valgrindCallgrindStart(SEXP);
extern SEXP _icd_valgrindCallgrindStop();
extern SEXP run_testthat_tests();

static const R_CallMethodDef CallEntries[] = {
    {"_icd_comorbidMatMul",                        (DL_FUNC) &_icd_comorbidMatMul,                        7},
    {"_icd_debug_parallel",                        (DL_FUNC) &_icd_debug_parallel,                        0},
    {"_icd_debug_parallel_env",                    (DL_FUNC) &_icd_debug_parallel_env,                    0},
    {"_icd_factor_fast",                           (DL_FUNC) &_icd_factor_fast,                           1},
    {"_icd_factorNoSort",                          (DL_FUNC) &_icd_factorNoSort,                          2},
    {"_icd_fastIntToString_alt_Std",               (DL_FUNC) &_icd_fastIntToString_alt_Std,               1},
    {"_icd_fastIntToStringRcpp",                   (DL_FUNC) &_icd_fastIntToStringRcpp,                   1},
    {"_icd_getOmpCores",                           (DL_FUNC) &_icd_getOmpCores,                           0},
    {"_icd_getOmpMaxThreads",                      (DL_FUNC) &_icd_getOmpMaxThreads,                      0},
    {"_icd_getOmpThreads",                         (DL_FUNC) &_icd_getOmpThreads,                         0},
    {"_icd_guessShortCompleteCpp",                 (DL_FUNC) &_icd_guessShortCompleteCpp,                 4},
    {"_icd_guessShortPlusFactorCpp",               (DL_FUNC) &_icd_guessShortPlusFactorCpp,               2},
    {"_icd_icd10cmChildrenDefined",                (DL_FUNC) &_icd_icd10cmChildrenDefined,                3},
    {"_icd_icd10ComorbidParentSearchCpp",          (DL_FUNC) &_icd_icd10ComorbidParentSearchCpp,          4},
    {"_icd_icd10DecimalToPartsCpp",                (DL_FUNC) &_icd_icd10DecimalToPartsCpp,                2},
    {"_icd_icd10ShortToPartsCpp",                  (DL_FUNC) &_icd_icd10ShortToPartsCpp,                  2},
    {"_icd_icd9_is_e_cpp",                         (DL_FUNC) &_icd_icd9_is_e_cpp,                         1},
    {"_icd_icd9_is_n_cpp",                         (DL_FUNC) &_icd_icd9_is_n_cpp,                         1},
    {"_icd_icd9_is_v_cpp",                         (DL_FUNC) &_icd_icd9_is_v_cpp,                         1},
    {"_icd_icd9AddLeadingZeroes",                  (DL_FUNC) &_icd_icd9AddLeadingZeroes,                  2},
    {"_icd_icd9AddLeadingZeroes_alt_Direct",       (DL_FUNC) &_icd_icd9AddLeadingZeroes_alt_Direct,       2},
    {"_icd_icd9AddLeadingZeroes_alt_ShortSingle",  (DL_FUNC) &_icd_icd9AddLeadingZeroes_alt_ShortSingle,  1},
    {"_icd_icd9AddLeadingZeroesMajor",             (DL_FUNC) &_icd_icd9AddLeadingZeroesMajor,             1},
    {"_icd_icd9AddLeadingZeroesMajorSingle",       (DL_FUNC) &_icd_icd9AddLeadingZeroesMajorSingle,       1},
    {"_icd_icd9AddLeadingZeroesMajorSingleStd",    (DL_FUNC) &_icd_icd9AddLeadingZeroesMajorSingleStd,    1},
    {"_icd_icd9AppendMinors",                      (DL_FUNC) &_icd_icd9AppendMinors,                      3},
    {"_icd_icd9Children_alt_ShortNoNaUnordered",   (DL_FUNC) &_icd_icd9Children_alt_ShortNoNaUnordered,   2},
    {"_icd_icd9ChildrenCpp",                       (DL_FUNC) &_icd_icd9ChildrenCpp,                       4},
    {"_icd_icd9ChildrenDecimalCpp",                (DL_FUNC) &_icd_icd9ChildrenDecimalCpp,                3},
    {"_icd_icd9ChildrenShort",                     (DL_FUNC) &_icd_icd9ChildrenShort,                     3},
    {"_icd_icd9ChildrenShort_alt_11",              (DL_FUNC) &_icd_icd9ChildrenShort_alt_11,              2},
    {"_icd_icd9ChildrenShort_alt_Std",             (DL_FUNC) &_icd_icd9ChildrenShort_alt_Std,             2},
    {"_icd_icd9ChildrenShortUnordered",            (DL_FUNC) &_icd_icd9ChildrenShortUnordered,            3},
    {"_icd_icd9Comorbid_alt_Taskloop",             (DL_FUNC) &_icd_icd9Comorbid_alt_Taskloop,             7},
    {"_icd_icd9Comorbid_alt_Taskloop2",            (DL_FUNC) &_icd_icd9Comorbid_alt_Taskloop2,            7},
    {"_icd_icd9ComorbidShortCpp",                  (DL_FUNC) &_icd_icd9ComorbidShortCpp,                  7},
    {"_icd_icd9DecimalToPartsCpp",                 (DL_FUNC) &_icd_icd9DecimalToPartsCpp,                 2},
    {"_icd_icd9DecimalToShort",                    (DL_FUNC) &_icd_icd9DecimalToShort,                    1},
    {"_icd_icd9ExpandMinor",                       (DL_FUNC) &_icd_icd9ExpandMinor,                       2},
    {"_icd_icd9ExpandMinor_alt_Std",               (DL_FUNC) &_icd_icd9ExpandMinor_alt_Std,               2},
    {"_icd_icd9GetMajor",                          (DL_FUNC) &_icd_icd9GetMajor,                          2},
    {"_icd_icd9LongToWideCpp",                     (DL_FUNC) &_icd_icd9LongToWideCpp,                     4},
    {"_icd_icd9MajMinToCode",                      (DL_FUNC) &_icd_icd9MajMinToCode,                      3},
    {"_icd_icd9MajMinToCode_alt_Old",              (DL_FUNC) &_icd_icd9MajMinToCode_alt_Old,              3},
    {"_icd_icd9MajMinToCode_alt_PrePadded",        (DL_FUNC) &_icd_icd9MajMinToCode_alt_PrePadded,        3},
    {"_icd_icd9MajMinToCode_alt_Std",              (DL_FUNC) &_icd_icd9MajMinToCode_alt_Std,              3},
    {"_icd_icd9MajMinToDecimal",                   (DL_FUNC) &_icd_icd9MajMinToDecimal,                   2},
    {"_icd_icd9MajMinToParts",                     (DL_FUNC) &_icd_icd9MajMinToParts,                     2},
    {"_icd_icd9MajMinToShort",                     (DL_FUNC) &_icd_icd9MajMinToShort,                     2},
    {"_icd_icd9MajMinToShort_alt_Std",             (DL_FUNC) &_icd_icd9MajMinToShort_alt_Std,             2},
    {"_icd_icd9MajMinToShortSingle_alt_Std",       (DL_FUNC) &_icd_icd9MajMinToShortSingle_alt_Std,       2},
    {"_icd_icd9OrderCpp",                          (DL_FUNC) &_icd_icd9OrderCpp,                          1},
    {"_icd_icd9PartsToDecimal",                    (DL_FUNC) &_icd_icd9PartsToDecimal,                    1},
    {"_icd_icd9PartsToShort",                      (DL_FUNC) &_icd_icd9PartsToShort,                      1},
    {"_icd_icd9RandomShort",                       (DL_FUNC) &_icd_icd9RandomShort,                       1},
    {"_icd_icd9RandomShortE",                      (DL_FUNC) &_icd_icd9RandomShortE,                      1},
    {"_icd_icd9RandomShortN",                      (DL_FUNC) &_icd_icd9RandomShortN,                      1},
    {"_icd_icd9RandomShortV",                      (DL_FUNC) &_icd_icd9RandomShortV,                      1},
    {"_icd_icd9ShortToDecimal",                    (DL_FUNC) &_icd_icd9ShortToDecimal,                    1},
    {"_icd_icd9ShortToPartsCpp",                   (DL_FUNC) &_icd_icd9ShortToPartsCpp,                   2},
    {"_icd_icd9SortCpp",                           (DL_FUNC) &_icd_icd9SortCpp,                           1},
    {"_icd_lookupComorbid_alt_ByChunkForTaskloop", (DL_FUNC) &_icd_lookupComorbid_alt_ByChunkForTaskloop, 3},
    {"_icd_lookupComorbidByChunkFor",              (DL_FUNC) &_icd_lookupComorbidByChunkFor,              5},
    {"_icd_randomMajorCpp",                        (DL_FUNC) &_icd_randomMajorCpp,                        1},
    {"_icd_RcppExport_registerCCallable",          (DL_FUNC) &_icd_RcppExport_registerCCallable,          0},
    {"_icd_setDecimalDiag",                        (DL_FUNC) &_icd_setDecimalDiag,                        2},
    {"_icd_setShortDiag",                          (DL_FUNC) &_icd_setShortDiag,                          2},
    {"_icd_simplifyMapLexicographic",              (DL_FUNC) &_icd_simplifyMapLexicographic,              2},
    {"_icd_strimCpp",                              (DL_FUNC) &_icd_strimCpp,                              1},
    {"_icd_trimCpp",                               (DL_FUNC) &_icd_trimCpp,                               1},
    {"_icd_trimLeftCpp",                           (DL_FUNC) &_icd_trimLeftCpp,                           1},
    {"_icd_valgrindCallgrindStart",                (DL_FUNC) &_icd_valgrindCallgrindStart,                1},
    {"_icd_valgrindCallgrindStop",                 (DL_FUNC) &_icd_valgrindCallgrindStop,                 0},
    {"run_testthat_tests",                         (DL_FUNC) &run_testthat_tests,                         0},
    {NULL, NULL, 0}
};

void R_init_icd(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
