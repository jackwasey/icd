#include "cutil.h"
#include <R.h>
#include <Rinternals.h>
#include <string.h>

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

void delanychar(char* str, char c) {
	char *pread = str, *pwrite = str;
	while (*pread) {
		*pwrite = *pread++;
		pwrite += (*pwrite != c);
	}
	*pwrite = '\0';
}

// this works, but with compiler warnings for const char * to char *
//void rvecdelchar(SEXP v) {
//        int vlen = length(v);
//        for (int i = 0; i != vlen; ++i) {
//		Rprintf("i = %d\n", i);
//                char* vr = CHAR(STRING_ELT(v, i));
//		Rprintf("vr = %s\n", vr);
//		char* vw = vr;
//		Rprintf("vr = %s\n", vw);
//      	while (*vr) {
//		Rprintf("vr address = %d\n", vr);
//		Rprintf("vw address = %d\n", vw);
//			*vw = *vr++;
//			vw += (*vw != '.');
//      	}
//		*vw = '\0';
//	}
//}

void dropdot(SEXP v) {
  PROTECT(v);
	int vlen = length(v);
	char temp[15]; // TODO: check length, or make buffer bigger? ICD codes should be maximum 6 characters, but could have whitespace or bad data...
	int i;
	for (i = 0; i != vlen; ++i) {
		if (STRING_ELT(v, i) == NA_STRING)
			continue;
		const char* vr = CHAR(STRING_ELT(v, i));
		if (strlen(vr) > 15)
			Rf_error("ICD code length greater than 15 characters");
		strcpy(temp, vr);
		char *vw = temp; // reuse the same buffer, but get fresh pointer
		while (*vr) {
			*vw = *vr++;
			vw += (*vw != '.');
		}
		*vw = '\0';
		// we can't write the c string directly to the R structure (but we can read the connsst char *)
		SET_STRING_ELT(v, i, mkChar(temp));
	}
	UNPROTECT(1);
}

