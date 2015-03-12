// [[Rcpp::interfaces(r, cpp)]]
#include <is.h>
#include <convert.h>
using namespace Rcpp;

// [[Rcpp::export]]
String icd9AddLeadingZeroesMajorSingle(String major) {
	if (major == NA_STRING) {
		return (NA_STRING);
	}
	std::string m(major);
	if (!icd9IsASingleVE(major.get_cstring())) {
		switch (strlen(major.get_cstring())) {
		case 0:
			return (NA_STRING);
		case 1:
			return ("00" + m);
		case 2:
			return ("0" + m);
		case 3:
			return (m);
		}
	} else {
		switch (strlen(major.get_cstring())) {
		case 1:
			return (NA_STRING);
		case 2:
			if (icd9IsASingleV(m.c_str())) {
				m.insert(1, "0");
				return (m);
			} else {
				m.insert(1, "00");
				return (m);
			}
		case 3:
			if (icd9IsASingleV(m.c_str())) {
				return (m);
			} else {
				m.insert(1, "0");
				return (m);
			}
		case 4:
			if (icd9IsASingleE(m.c_str()))
				return (m);
		}
	}
	return NA_STRING;
}

//' @rdname icd9AddLeadingZeroes
// [[Rcpp::export]]
CharacterVector icd9AddLeadingZeroesMajor(CharacterVector major) {
	return sapply(major, icd9AddLeadingZeroesMajorSingle);
}

//' @rdname icd9AddLeadingZeroes
// [[Rcpp::export]]
CharacterVector icd9AddLeadingZeroesShort(CharacterVector icd9Short) {
	List parts = icd9ShortToParts(icd9Short);
	parts["major"] = icd9AddLeadingZeroesMajor(parts["major"]);
	return icd9PartsToShort(parts);
}

//' @rdname icd9AddLeadingZeroes
// [[Rcpp::export]]
CharacterVector icd9AddLeadingZeroesDecimal(CharacterVector icd9Decimal) {
	List parts = icd9DecimalToParts(icd9Decimal);
	parts["major"] = icd9AddLeadingZeroesMajor(
			as<CharacterVector>(parts["major"]));
	return icd9PartsToDecimal(parts);
}

//' @title Add leading zeroes to incomplete ICD codes
//' @description Non-decimal ICD-9 codes with length<5 are often ambiguous. E.g.
//'   100 could be 1.00 10.0 or 100 if coded incorrectly. We must assume 100 is
//'   really 100
//' @template icd9-any
//' @template icd9-short
//' @template icd9-decimal
//' @template major
//' @return character vector of ICD codes (or major part thereof)
//' @keywords internal manip
// [[Rcpp::export]]
CharacterVector icd9AddLeadingZeroes(CharacterVector icd9, bool isShort) {
	if (isShort)
		return icd9AddLeadingZeroesShort(icd9);
	return icd9AddLeadingZeroesDecimal(icd9);
}
