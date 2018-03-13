#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _icd_fastIntToStringRcpp(SEXP);
extern SEXP _icd_fastIntToStringStd(SEXP);
extern SEXP _icd_getOmpCores();
extern SEXP _icd_getOmpMaxThreads();
extern SEXP _icd_getOmpThreads();
extern SEXP _icd_guessShortCompleteCpp(SEXP, SEXP, SEXP, SEXP);
extern SEXP _icd_guessShortPlusFactorCpp(SEXP, SEXP);
extern SEXP _icd_icd_in_reference_code(SEXP, SEXP, SEXP, SEXP);
extern SEXP _icd_icd10_comorbid_parent_search_cpp(SEXP, SEXP, SEXP, SEXP);
extern SEXP _icd_icd10cmChildrenDefined(SEXP);
extern SEXP _icd_icd10DecimalToPartsCpp(SEXP, SEXP);
extern SEXP _icd_icd10ShortToPartsCpp(SEXP, SEXP);
extern SEXP _icd_icd9_is_e_cpp(SEXP);
extern SEXP _icd_icd9_is_n_cpp(SEXP);
extern SEXP _icd_icd9_is_v_cpp(SEXP);
extern SEXP _icd_icd9AddLeadingZeroes(SEXP, SEXP);
extern SEXP _icd_icd9AddLeadingZeroesDirect(SEXP, SEXP);
extern SEXP _icd_icd9AddLeadingZeroesMajor(SEXP);
extern SEXP _icd_icd9AddLeadingZeroesMajorSingle(SEXP);
extern SEXP _icd_icd9AddLeadingZeroesMajorSingleStd(SEXP);
extern SEXP _icd_icd9AddLeadingZeroesShortSingle(SEXP);
extern SEXP _icd_icd9AppendMinors(SEXP, SEXP, SEXP);
extern SEXP _icd_icd9ChildrenCpp(SEXP, SEXP, SEXP);
extern SEXP _icd_icd9ChildrenDecimalCpp(SEXP, SEXP);
extern SEXP _icd_icd9ChildrenShort(SEXP, SEXP);
extern SEXP _icd_icd9ChildrenShort11(SEXP, SEXP);
extern SEXP _icd_icd9ChildrenShortNoNaUnordered(SEXP, SEXP);
extern SEXP _icd_icd9ChildrenShortStd(SEXP, SEXP);
extern SEXP _icd_icd9ChildrenShortUnordered(SEXP, SEXP);
extern SEXP _icd_icd9ComorbidShortCpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _icd_icd9ComorbidTaskloop(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _icd_icd9DecimalToPartsCpp(SEXP, SEXP);
extern SEXP _icd_icd9DecimalToShort(SEXP);
extern SEXP _icd_icd9ExpandMinor(SEXP, SEXP);
extern SEXP _icd_icd9ExpandMinorStd(SEXP, SEXP);
extern SEXP _icd_icd9GetMajor(SEXP, SEXP);
extern SEXP _icd_icd9LongToWideCpp(SEXP, SEXP, SEXP, SEXP);
extern SEXP _icd_icd9MajMinToCode(SEXP, SEXP, SEXP);
extern SEXP _icd_icd9MajMinToCodeOld(SEXP, SEXP, SEXP);
extern SEXP _icd_icd9MajMinToCodePrePadded(SEXP, SEXP, SEXP);
extern SEXP _icd_icd9MajMinToCodeStd(SEXP, SEXP, SEXP);
extern SEXP _icd_icd9MajMinToDecimal(SEXP, SEXP);
extern SEXP _icd_icd9MajMinToParts(SEXP, SEXP);
extern SEXP _icd_icd9MajMinToShort(SEXP, SEXP);
extern SEXP _icd_icd9MajMinToShortSingleStd(SEXP, SEXP);
extern SEXP _icd_icd9MajMinToShortStd(SEXP, SEXP);
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
extern SEXP _icd_lookupComorbidByChunkFor(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _icd_lookupComorbidByChunkForTaskloop(SEXP, SEXP, SEXP);
extern SEXP _icd_randomMajorCpp(SEXP);
extern SEXP _icd_RcppExport_registerCCallable();
extern SEXP _icd_setDecimalDiag(SEXP, SEXP);
extern SEXP _icd_setShortDiag(SEXP, SEXP);
extern SEXP _icd_strimCpp(SEXP);
extern SEXP _icd_trimCpp(SEXP);
extern SEXP _icd_trimLeftCpp(SEXP);
extern SEXP _icd_valgrindCallgrindStart(SEXP);
extern SEXP _icd_valgrindCallgrindStop();
extern SEXP run_testthat_tests();

static const R_CallMethodDef CallEntries[] = {
    {"_icd_fastIntToStringRcpp",                (DL_FUNC) &_icd_fastIntToStringRcpp,                1},
    {"_icd_fastIntToStringStd",                 (DL_FUNC) &_icd_fastIntToStringStd,                 1},
    {"_icd_getOmpCores",                        (DL_FUNC) &_icd_getOmpCores,                        0},
    {"_icd_getOmpMaxThreads",                   (DL_FUNC) &_icd_getOmpMaxThreads,                   0},
    {"_icd_getOmpThreads",                      (DL_FUNC) &_icd_getOmpThreads,                      0},
    {"_icd_guessShortCompleteCpp",              (DL_FUNC) &_icd_guessShortCompleteCpp,              4},
    {"_icd_guessShortPlusFactorCpp",            (DL_FUNC) &_icd_guessShortPlusFactorCpp,            2},
    {"_icd_icd_in_reference_code",              (DL_FUNC) &_icd_icd_in_reference_code,              4},
    {"_icd_icd10_comorbid_parent_search_cpp",   (DL_FUNC) &_icd_icd10_comorbid_parent_search_cpp,   4},
    {"_icd_icd10cmChildrenDefined",             (DL_FUNC) &_icd_icd10cmChildrenDefined,             1},
    {"_icd_icd10DecimalToPartsCpp",             (DL_FUNC) &_icd_icd10DecimalToPartsCpp,             2},
    {"_icd_icd10ShortToPartsCpp",               (DL_FUNC) &_icd_icd10ShortToPartsCpp,               2},
    {"_icd_icd9_is_e_cpp",                      (DL_FUNC) &_icd_icd9_is_e_cpp,                      1},
    {"_icd_icd9_is_n_cpp",                      (DL_FUNC) &_icd_icd9_is_n_cpp,                      1},
    {"_icd_icd9_is_v_cpp",                      (DL_FUNC) &_icd_icd9_is_v_cpp,                      1},
    {"_icd_icd9AddLeadingZeroes",               (DL_FUNC) &_icd_icd9AddLeadingZeroes,               2},
    {"_icd_icd9AddLeadingZeroesDirect",         (DL_FUNC) &_icd_icd9AddLeadingZeroesDirect,         2},
    {"_icd_icd9AddLeadingZeroesMajor",          (DL_FUNC) &_icd_icd9AddLeadingZeroesMajor,          1},
    {"_icd_icd9AddLeadingZeroesMajorSingle",    (DL_FUNC) &_icd_icd9AddLeadingZeroesMajorSingle,    1},
    {"_icd_icd9AddLeadingZeroesMajorSingleStd", (DL_FUNC) &_icd_icd9AddLeadingZeroesMajorSingleStd, 1},
    {"_icd_icd9AddLeadingZeroesShortSingle",    (DL_FUNC) &_icd_icd9AddLeadingZeroesShortSingle,    1},
    {"_icd_icd9AppendMinors",                   (DL_FUNC) &_icd_icd9AppendMinors,                   3},
    {"_icd_icd9ChildrenCpp",                    (DL_FUNC) &_icd_icd9ChildrenCpp,                    3},
    {"_icd_icd9ChildrenDecimalCpp",             (DL_FUNC) &_icd_icd9ChildrenDecimalCpp,             2},
    {"_icd_icd9ChildrenShort",                  (DL_FUNC) &_icd_icd9ChildrenShort,                  2},
    {"_icd_icd9ChildrenShort11",                (DL_FUNC) &_icd_icd9ChildrenShort11,                2},
    {"_icd_icd9ChildrenShortNoNaUnordered",     (DL_FUNC) &_icd_icd9ChildrenShortNoNaUnordered,     2},
    {"_icd_icd9ChildrenShortStd",               (DL_FUNC) &_icd_icd9ChildrenShortStd,               2},
    {"_icd_icd9ChildrenShortUnordered",         (DL_FUNC) &_icd_icd9ChildrenShortUnordered,         2},
    {"_icd_icd9ComorbidShortCpp",               (DL_FUNC) &_icd_icd9ComorbidShortCpp,               8},
    {"_icd_icd9ComorbidTaskloop",               (DL_FUNC) &_icd_icd9ComorbidTaskloop,               8},
    {"_icd_icd9DecimalToPartsCpp",              (DL_FUNC) &_icd_icd9DecimalToPartsCpp,              2},
    {"_icd_icd9DecimalToShort",                 (DL_FUNC) &_icd_icd9DecimalToShort,                 1},
    {"_icd_icd9ExpandMinor",                    (DL_FUNC) &_icd_icd9ExpandMinor,                    2},
    {"_icd_icd9ExpandMinorStd",                 (DL_FUNC) &_icd_icd9ExpandMinorStd,                 2},
    {"_icd_icd9GetMajor",                       (DL_FUNC) &_icd_icd9GetMajor,                       2},
    {"_icd_icd9LongToWideCpp",                  (DL_FUNC) &_icd_icd9LongToWideCpp,                  4},
    {"_icd_icd9MajMinToCode",                   (DL_FUNC) &_icd_icd9MajMinToCode,                   3},
    {"_icd_icd9MajMinToCodeOld",                (DL_FUNC) &_icd_icd9MajMinToCodeOld,                3},
    {"_icd_icd9MajMinToCodePrePadded",          (DL_FUNC) &_icd_icd9MajMinToCodePrePadded,          3},
    {"_icd_icd9MajMinToCodeStd",                (DL_FUNC) &_icd_icd9MajMinToCodeStd,                3},
    {"_icd_icd9MajMinToDecimal",                (DL_FUNC) &_icd_icd9MajMinToDecimal,                2},
    {"_icd_icd9MajMinToParts",                  (DL_FUNC) &_icd_icd9MajMinToParts,                  2},
    {"_icd_icd9MajMinToShort",                  (DL_FUNC) &_icd_icd9MajMinToShort,                  2},
    {"_icd_icd9MajMinToShortSingleStd",         (DL_FUNC) &_icd_icd9MajMinToShortSingleStd,         2},
    {"_icd_icd9MajMinToShortStd",               (DL_FUNC) &_icd_icd9MajMinToShortStd,               2},
    {"_icd_icd9OrderCpp",                       (DL_FUNC) &_icd_icd9OrderCpp,                       1},
    {"_icd_icd9PartsToDecimal",                 (DL_FUNC) &_icd_icd9PartsToDecimal,                 1},
    {"_icd_icd9PartsToShort",                   (DL_FUNC) &_icd_icd9PartsToShort,                   1},
    {"_icd_icd9RandomShort",                    (DL_FUNC) &_icd_icd9RandomShort,                    1},
    {"_icd_icd9RandomShortE",                   (DL_FUNC) &_icd_icd9RandomShortE,                   1},
    {"_icd_icd9RandomShortN",                   (DL_FUNC) &_icd_icd9RandomShortN,                   1},
    {"_icd_icd9RandomShortV",                   (DL_FUNC) &_icd_icd9RandomShortV,                   1},
    {"_icd_icd9ShortToDecimal",                 (DL_FUNC) &_icd_icd9ShortToDecimal,                 1},
    {"_icd_icd9ShortToPartsCpp",                (DL_FUNC) &_icd_icd9ShortToPartsCpp,                2},
    {"_icd_icd9SortCpp",                        (DL_FUNC) &_icd_icd9SortCpp,                        1},
    {"_icd_lookupComorbidByChunkFor",           (DL_FUNC) &_icd_lookupComorbidByChunkFor,           5},
    {"_icd_lookupComorbidByChunkForTaskloop",   (DL_FUNC) &_icd_lookupComorbidByChunkForTaskloop,   3},
    {"_icd_randomMajorCpp",                     (DL_FUNC) &_icd_randomMajorCpp,                     1},
    {"_icd_RcppExport_registerCCallable",       (DL_FUNC) &_icd_RcppExport_registerCCallable,       0},
    {"_icd_setDecimalDiag",                     (DL_FUNC) &_icd_setDecimalDiag,                     2},
    {"_icd_setShortDiag",                       (DL_FUNC) &_icd_setShortDiag,                       2},
    {"_icd_strimCpp",                           (DL_FUNC) &_icd_strimCpp,                           1},
    {"_icd_trimCpp",                            (DL_FUNC) &_icd_trimCpp,                            1},
    {"_icd_trimLeftCpp",                        (DL_FUNC) &_icd_trimLeftCpp,                        1},
    {"_icd_valgrindCallgrindStart",             (DL_FUNC) &_icd_valgrindCallgrindStart,             1},
    {"_icd_valgrindCallgrindStop",              (DL_FUNC) &_icd_valgrindCallgrindStop,              0},
    {"run_testthat_tests",                      (DL_FUNC) &run_testthat_tests,                      0},
    {NULL, NULL, 0}
};

void R_init_icd(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
