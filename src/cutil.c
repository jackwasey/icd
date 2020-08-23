#include "cutil.h"
#include <R.h>
#include <Rinternals.h>
#include <string.h>

SEXP getRListOrDfElement(SEXP list_or_df, const char* element_name) {
  SEXP names   = PROTECT(Rf_getAttrib(list_or_df, R_NamesSymbol));
  SEXP element = PROTECT(R_NilValue);
  int i;
  int len = Rf_length(list_or_df);
  for (i = 0; i < len; i++) {
    if (strcmp(CHAR(STRING_ELT(names, i)), element_name) == 0) {
      element = VECTOR_ELT(list_or_df, i);
      break;
    }
  }
  UNPROTECT(2);
  return element;
}

// similar to the R source function in main/coerce.c, but without any checks.
SEXP factorAsChar(SEXP x) {
  if (!Rf_isFactor(x)) Rf_error("icd C function factorAsChar called with non-factor");
  SEXP out;
  R_xlen_t i;
  R_xlen_t n = Rf_xlength(x);
  SEXP lvls  = getAttrib(x, R_LevelsSymbol);
  PROTECT(out = allocVector(STRSXP, n));
  for (i = 0; i < n; i++) {
    int v = INTEGER_ELT(x, i);
    if (v == NA_INTEGER)
      SET_STRING_ELT(out, i, NA_STRING);
    else
      SET_STRING_ELT(out, i, STRING_ELT(lvls, v - 1));
  }
  UNPROTECT(1); // out
  return out;
}

// unsafe if there might be NA values
SEXP factorAsCharNoNa(SEXP x) {
  if (!Rf_isFactor(x)) Rf_error("factorAsCharNoNa called with non-factor");
  SEXP out;
  R_xlen_t i;
  R_xlen_t n = Rf_xlength(x);
  SEXP lvls  = getAttrib(x, R_LevelsSymbol);
  PROTECT(out = allocVector(STRSXP, n));
  for (i = 0; i != n; ++i) {
    int v = INTEGER_ELT(x, i);
    SET_STRING_ELT(out, i, STRING_ELT(lvls, v - 1));
  }
  UNPROTECT(1); // out
  return out;
}
