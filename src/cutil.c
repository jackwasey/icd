#include "cutil.h"
#include <R.h>
#include <Rinternals.h>
#include <string.h>

SEXP getRListOrDfElement(SEXP list_or_df, const char* element_name) {
  SEXP names = PROTECT(Rf_getAttrib(list_or_df, R_NamesSymbol));
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
