// Copyright (C) 2014 - 2015  Jack O. Wasey
//
// This file is part of icd9.
//
// icd9 is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// icd9 is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with icd9. If not, see <http://www.gnu.org/licenses/>.

// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include "manip.h"
#include "is.h"
#include "convert.h"

// [[Rcpp::export]]
Rcpp::String icd9AddLeadingZeroesMajorSingle(Rcpp::String major) {
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

// [[Rcpp::export]]
std::string icd9AddLeadingZeroesMajorSingleStd(std::string m) {
	const char * cs = m.c_str();
	const std::string::size_type len = m.length();
	if (!icd9IsASingleVE(cs)) {
		switch (len) {
		case 0:
			return ("");
		case 1:
			return ("00" + m);
		case 2:
			return ("0" + m);
		case 3:
			return (m);
		}
	} else {
		switch (len) {
		case 1:
			return ("");
		case 2:
			if (icd9IsASingleV(cs)) {
				m.insert(1, "0");
				return (m);
			} else {
				m.insert(1, "00");
				return (m);
			}
		case 3:
			if (icd9IsASingleV(cs)) {
				return (m);
			} else {
				m.insert(1, "0");
				return (m);
			}
		case 4:
			if (icd9IsASingleE(cs))
				return (m);
		}
	}
	return "";
}

// [[Rcpp::export(icd9_add_leading_zeroes_major)]]
Rcpp::CharacterVector icd9AddLeadingZeroesMajor(Rcpp::CharacterVector major) {
	return Rcpp::sapply(major, icd9AddLeadingZeroesMajorSingle);
}

//' @rdname icd9AddLeadingZeroes
// [[Rcpp::export(icd9_add_leading_zeroes.icd_short_code)]]
Rcpp::CharacterVector icd9AddLeadingZeroesShort(
		Rcpp::CharacterVector x) {
	Rcpp::List parts = icd9ShortToPartsCpp(x, "");
	parts["major"] = icd9AddLeadingZeroesMajor(parts["major"]);
	return icd9PartsToShort(parts);
}

//' @rdname icd9AddLeadingZeroes
// [[Rcpp::export(icd9_add_leading_zeroes.icd_decimal_code)]]
Rcpp::CharacterVector icd9AddLeadingZeroesDecimal(
		Rcpp::CharacterVector x) {
	Rcpp::List parts = icd9DecimalToPartsCpp(x);
	parts["major"] = icd9AddLeadingZeroesMajor(
			Rcpp::as<Rcpp::CharacterVector>(parts["major"]));
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
Rcpp::CharacterVector icd9AddLeadingZeroes(Rcpp::CharacterVector icd9,
		bool isShort) {
	if (isShort)
		return icd9AddLeadingZeroesShort(icd9);
	return icd9AddLeadingZeroesDecimal(icd9);
}
