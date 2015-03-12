//#include <util.h>
#include <string.h>
#include <Rinternals.h>

SEXP getRListOrDfElement(SEXP list_or_df, const char* element_name) {
	SEXP names = Rf_getAttrib(list_or_df, R_NamesSymbol);
	SEXP element = R_NilValue;
	int i;
	for (i = 0; i < Rf_length(list_or_df); i++) {
		if (strcmp(CHAR(STRING_ELT(names, i)), element_name) == 0) {
			element = VECTOR_ELT(list_or_df, i);
			break;
		}
	}
	return element;
}

