#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/*
  The following symbols/expressions for .NAME have been omitted

    _icd_icd9MajMinToCode
    _icd_icd9MajMinToCodePrePadded
    _icd_icd9MajMinToCodeStd
    _icd_icd9MajMinToShort
    _icd_icd9MajMinToDecimal
    _icd_icd9AppendMinors
    _icd_icd9MajMinToShortStd
    _icd_icd9MajMinToShortSingleStd
    _icd_setDecimalDiag
    _icd_setShortDiag
    _icd_icd10cmChildrenDefined
    _icd_icd9ComorbidShortCpp
    _icd_lookupComorbidByChunkFor
    _icd_icd10_comorbid_parent_search_cpp
    _icd_icd9PartsToShort
    _icd_icd9PartsToDecimal
    _icd_icd9MajMinToParts
    _icd_icd9ShortToPartsCpp
    _icd_icd10ShortToPartsCpp
    _icd_icd9DecimalToPartsCpp
    _icd_icd10DecimalToPartsCpp
    _icd_icd9ShortToDecimal
    _icd_icd9DecimalToShort
    _icd_icd9GetMajor
    _icd_icd9MajMinToCodeOld
    _icd_fastIntToStringStd
    _icd_fastIntToStringRcpp
    _icd_guessShortCompleteCpp
    _icd_guessShortPlusFactorCpp
    _icd_icd9_is_n_cpp
    _icd_icd9_is_v_cpp
    _icd_icd9_is_e_cpp
    _icd_icd9LongToWideCpp
    _icd_icd9AddLeadingZeroesMajorSingle
    _icd_icd9AddLeadingZeroesMajorSingleStd
    _icd_icd9AddLeadingZeroesMajor
    _icd_icd9AddLeadingZeroes
    _icd_icd9AddLeadingZeroesShortSingle
    _icd_icd9AddLeadingZeroesDirect
    _icd_icd9ExpandMinorStd
    _icd_icd9ExpandMinor
    _icd_icd9ChildrenShort
    _icd_icd9ChildrenShortUnordered
    _icd_icd9ChildrenShortNoNaUnordered
    _icd_icd9ChildrenDecimalCpp
    _icd_icd9ChildrenCpp
    _icd_icd_in_reference_code
    _icd_icd9ChildrenShort11
    _icd_icd9ChildrenShortStd
    _icd_trimLeftCpp
    _icd_strimCpp
    _icd_trimCpp
    _icd_getOmpCores
    _icd_getOmpMaxThreads
    _icd_getOmpThreads
    _icd_randomMajorCpp
    _icd_icd9RandomShortN
    _icd_icd9RandomShortV
    _icd_icd9RandomShortE
    _icd_icd9RandomShort
    _icd_valgrindCallgrindStart
    _icd_valgrindCallgrindStop
    _icd_icd9SortCpp
    _icd_icd9OrderCpp

  Most likely possible values need to be added below.
*/

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _icd_icd9ChildrenDecimalCpp(SEXP, SEXP);
extern SEXP _icd_icd9ChildrenShortUnordered(SEXP, SEXP);
extern SEXP _icd_icd9DecimalToPartsCpp(SEXP, SEXP);
extern SEXP _icd_icd9ShortToPartsCpp(SEXP, SEXP);
extern SEXP _icd_RcppExport_registerCCallable();
extern SEXP _icd_strimCpp(SEXP);
extern SEXP _icd_trimCpp(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_icd_icd9ChildrenDecimalCpp",       (DL_FUNC) &_icd_icd9ChildrenDecimalCpp,       2},
    {"_icd_icd9ChildrenShortUnordered",   (DL_FUNC) &_icd_icd9ChildrenShortUnordered,   2},
    {"_icd_icd9DecimalToPartsCpp",        (DL_FUNC) &_icd_icd9DecimalToPartsCpp,        2},
    {"_icd_icd9ShortToPartsCpp",          (DL_FUNC) &_icd_icd9ShortToPartsCpp,          2},
    {"_icd_RcppExport_registerCCallable", (DL_FUNC) &_icd_RcppExport_registerCCallable, 0},
    {"_icd_strimCpp",                     (DL_FUNC) &_icd_strimCpp,                     1},
    {"_icd_trimCpp",                      (DL_FUNC) &_icd_trimCpp,                      1},
    {NULL, NULL, 0}
};

void R_init_icd(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
