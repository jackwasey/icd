// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include <R.h>
#include <Rinternals.h>
#include <icd9.h>
#include <local.h>
//#include <boost/unordered/unordered_set.hpp>
extern "C" {
#include "local_c.h"
}
using namespace Rcpp;

//' @rdname convert
//' @name convert
//' @export
// [[Rcpp::export]]
CharacterVector icd9MajMinToCode(const CharacterVector major, const CharacterVector minor, bool isShort) {

	CharacterVector out;
	CharacterVector::const_iterator j = major.begin();
	CharacterVector::const_iterator n = minor.begin();

	for (; j != major.end() && n != minor.end(); ++j, ++n) {
		//std::string majorelem = as<std::string>(*j);
		String majorelem = *j;
		if (majorelem == NA_STRING) {
			out.push_back(NA_STRING);
			continue;
		}
		// work around Rcpp bug with push_front: convert to string just for this
		std::string smj = std::string(majorelem.get_cstring());
		switch (strlen(majorelem.get_cstring())) {
		case 0:
			out.push_back(NA_STRING);
			continue;
		case 1:
			if (!icd9::icd9IsASingleVE(majorelem)) {
				smj.insert(0, "00");
			}
			break;
		case 2:
			if (!icd9::icd9IsASingleVE(majorelem)) {
				smj.insert(0, "0");
			} else {
				smj.insert(1, "0");
			}
			// default: // major is 3 (or more) chars already
		}
		String minorelem = *n;
		if (minorelem == NA_STRING) {
			//out.push_back(majorelem);
			out.push_back(smj);
			continue;
		}
		// this can probably be done more quickly:
		//std::string smj(majorelem);
		if (!isShort && minorelem != "") {
			smj.append(".");
		}
		smj.append(minorelem);
		out.push_back(smj);

	}
	// String chr_s = NA_STRING; // TODO

	return wrap(out);
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
CharacterVector icd9MajMinToShort(const CharacterVector major,
		const CharacterVector minor) {
	if ((major.size() != 1 && major.size() != minor.size())
			|| (major.size() == 1 && minor.size() == 0)) {
		stop("icd9MajMinToShort, length of majors and minors must be equal.");
	}
	if (major.size() == 1) {
		CharacterVector newmajor(minor.size(), major[0]);
		return icd9MajMinToCode(newmajor, minor, true);
	}
	return icd9MajMinToCode(major, minor, true);
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
CharacterVector icd9MajMinToDecimal(const CharacterVector major,
		const CharacterVector minor) {
	return icd9MajMinToCode(major, minor, false);
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
CharacterVector icd9PartsToShort(const List parts) {
	return icd9MajMinToCode(parts["major"], parts["minor"], true);
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
CharacterVector icd9PartsToDecimal(const List parts) {
	return icd9MajMinToCode(parts["major"], parts["minor"], false);
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
List icd9MajMinToParts(const CharacterVector major, const CharacterVector minor) {
	List returned_frame = List::create(_["major"] = major, _["minor"] = minor);

	// TODO: can do this with lists, ?no need for a data frame
	StringVector sample_row = returned_frame(0);
	IntegerVector row_names = seq_along(sample_row);
	returned_frame.attr("row.names") = row_names;
	returned_frame.attr("class") = "data.frame";

	return returned_frame;
}

// this is even faster, but loses some useful data frame features which cause test failure
// [[Rcpp::export]]
List icd9MajMinToParts_list(const CharacterVector major, const CharacterVector minor) {
	List out = List::create(_["major"] = major, _["minor"] = minor);
	return out;
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
List icd9ShortToParts(const CharacterVector icd9Short, const String minorEmpty = "") {

	CharacterVector major(icd9Short.size());
	CharacterVector minor(icd9Short.size());

	for (int i = 0; i < icd9Short.size(); ++i) {
		if (CharacterVector::is_na(icd9Short[i])) {
			minor[i] = NA_STRING;
			continue;
		}

		std::string s = as<std::string>(icd9Short[i]);
		s = icd9::strim_cpp(s); // do i need to convert?

		if (!icd9::icd9IsASingleE(s)) { // not an E code
			switch (s.size()) {
			case 1:
			case 2:
			case 3:
				major[i] = s.substr(0, s.size());
				minor[minorEmpty];
				continue;
			case 4:
			case 5:
				major[i] = s.substr(0, 3);
				minor[i] = s.substr(3, s.size() - 3);
				continue;
			default:
				major[i] = NA_STRING;
				minor[i] = NA_STRING;
				continue;
			}
		} else { // E code

			switch (s.size()) {
			case 2:
			case 3:
			case 4:
				major[i] = s.substr(0, s.size());
				minor[i] = minorEmpty;
				break;
			case 5:
				major[i] = s.substr(0, 4);
				minor[i] = s.substr(4, 1);
				break;
			default:
				major[i] = NA_STRING;
				minor[i] = NA_STRING;
				continue;
			}
		} // E code
		//major[i] = icd9::icd9AddLeadingZeroesMajorSingle(major[i]); // or loop through them all again...
	} // for

	return icd9MajMinToParts(icd9::icd9AddLeadingZeroesMajor(major), minor);
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
List icd9DecimalToParts(const CharacterVector icd9Decimal, const String minorEmpty = "") {
	CharacterVector majors;
	CharacterVector minors;
	int ilen = icd9Decimal.length();

	if (ilen == 0) {
		return List::create(_["major"] = CharacterVector::create(), _["minor"] =
				CharacterVector::create());
	}

	for (CharacterVector::const_iterator it = icd9Decimal.begin();
			it != icd9Decimal.end(); ++it) {
		String strna = *it;
		if (strna == NA_STRING || strna == "") {
			majors.push_back(NA_STRING);
			minors.push_back(NA_STRING);
			continue;
		}
		std::string thiscode = as<std::string>(*it); // Rcpp::String doesn't implement many functions.
		thiscode = icd9::strim_cpp(thiscode); // TODO: update in place.
		std::size_t pos = thiscode.find(".");
		// substring parts
		std::string majorin;
		String minorout;
		if (pos != std::string::npos) {
			majorin = thiscode.substr(0, pos);
			minorout = thiscode.substr(pos + 1);
		} else {
			majorin = thiscode;
			minorout = minorEmpty;
		}
		majors.push_back(icd9::icd9AddLeadingZeroesMajorSingle(majorin));
		minors.push_back(minorout);
	}
	return List::create(_["major"] = majors, _["minor"] = minors);
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
CharacterVector icd9ShortToDecimal(const CharacterVector icd9Short) {
	return icd9PartsToDecimal(icd9ShortToParts(icd9Short));
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
CharacterVector icd9DecimalToShort(const CharacterVector icd9Decimal) {
	return icd9PartsToShort(icd9DecimalToParts(icd9Decimal));
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
CharacterVector icd9GetMajor(const CharacterVector icd9, const bool isShort) {
	if (isShort) {
		return as<CharacterVector>(icd9ShortToParts(icd9)[0]);
	}
	return as<CharacterVector>(icd9DecimalToParts(icd9)[0]);
}

template<typename T>
SEXP raggedWideVecVecToMatrix(const std::vector<VecStr>& ragged,
		unsigned int max_per_pt, const T &visitIds) {
#ifdef ICD9_DEBUG_SETUP
	std::cout << "visitIds = ";
	printIt(visitIds);
#endif
	unsigned int distinct_visits = ragged.size();
	CharacterVector out(distinct_visits * max_per_pt); // default empty strings? NA? //TODO
	for (unsigned int row_it = 0; row_it != distinct_visits; ++row_it) {
		const VecStr& this_row = ragged[row_it];
		unsigned int this_row_len = this_row.size();
		for (unsigned int col_it = 0; col_it < this_row_len; ++col_it) {
			unsigned int out_idx = row_it + (distinct_visits * col_it); // straight to row major //TODO benchmark alternative with transposition
			out[out_idx] = this_row[col_it];
		}
	}
#ifdef ICD9_DEBUG_SETUP
	std::cout << "writing dimensions\n";
#endif
	out.attr("dim") = Dimension(distinct_visits, max_per_pt); // set dimensions in reverse (row major for parallel step)
#ifdef ICD9_DEBUG_SETUP
	std::cout << "writing labels\n";
#endif
	CharacterVector nonames;
	rownames(out) = wrap(visitIds);
	return out;
}

SEXP raggedWideMultimapToMatrix(const MMVisitCodes &mm, const unsigned int max_per_pt) {
#ifdef ICD9_DEBUG_SETUP
	//std::cout << "visitIds = ";
	//printIt(visitIds);
#endif
	unsigned int distinct_visits = mm.size();
	CharacterVector out(distinct_visits * max_per_pt); // default empty strings? NA? //TODO
	for (MMVisitCodes::const_iterator it=mm.begin(); it!=mm.end(); ++it) {
		unsigned int this_row_len = (it->second).size();
		const unsigned int row_idx=std::distance(mm.begin(), it);
		for (unsigned int col_idx = 0; col_idx < this_row_len; ++col_idx) {
			unsigned int out_idx = row_idx + (distinct_visits * col_idx); // straight to row major //TODO benchmark alternative with transposition
			out[out_idx] = (it->second)[col_idx];
		}
	}
#ifdef ICD9_DEBUG_SETUP
	std::cout << "writing dimensions\n";
#endif
	out.attr("dim") = Dimension(distinct_visits, max_per_pt); // set dimensions in reverse (row major for parallel step)
#ifdef ICD9_DEBUG_SETUP
	std::cout << "writing labels\n";
#endif
	CharacterVector nonames;
	//rownames(out) = wrap(visitIds);
	return out;
}

// [[Rcpp::export]]
SEXP icd9LongToWideMatrixByMap(const SEXP& icd9df, const std::string visitId =
		"visitId", const std::string icd9Field = "icd9") {

#ifdef ICD9_DEBUG_SETUP
	std::cout << "calling C to get icd codes\n";
#endif
	SEXP icds = getListElement(icd9df, icd9Field.c_str());
#ifdef ICD9_DEBUG_SETUP
	std::cout << "back from C\n";
#endif
	VecStr vs = as<VecStr>(
			as<CharacterVector>(getListElement(icd9df, visitId.c_str()))); // TODO can we do this in one step without Rcpp copying?
	const unsigned int approx_cmb_per_visit = 5; // just an estimate
#ifdef ICD9_DEBUG_SETUP
	std::cout << "getting length of icd codes\n";
#endif
	unsigned int vlen = Rf_length(icds);
	MMVisitCodes visitCodes;
	unsigned int max_per_pt = 1;
	Str lastVisitId;
	MMVisitCodes::iterator last_found_it;
	for (unsigned int i = 0; i < vlen; ++i) {
#ifdef ICD9_DEBUG_SETUP_TRACE
		std::cout << "calling R C function to get current ICD...";
#endif
		const char* s = CHAR(STRING_ELT(icds, i));
#ifdef ICD9_DEBUG_SETUP_TRACE
		std::cout << " and got value: " << s << "\n";
		std::cout << "visitIds = ";
		//printIt(visitIds);
		std::cout << "Current visitId: " << vs[i] << "\n";
#endif
		if (lastVisitId==s) { // shortcut for overwhelmingly most frequent case
#ifdef ICD9_DEBUG_SETUP_TRACE
				std::cout << "in-sequence repeat id found: " << vs[i] << "\n";
#endif

			(last_found_it->second).push_back(s);
			//TODO: avoid the slow lookup here?
			unsigned int len = (last_found_it->second).size(); // get new count of cmb for one patient
			if (len > max_per_pt)
				max_per_pt = len;
		} else {
			MMVisitCodes::iterator found_it = visitCodes.find(vs[i]);
			if (found_it != visitCodes.end()) {
#ifdef ICD9_DEBUG_SETUP_TRACE
				std::cout << "out-of-sequence repeat id found: " << vs[i] << "\n";
				std::cout << "map size: " << visitCodes.size() << "\n";
#endif
				(found_it->second).push_back(s); // augment vec for current visit and N/V/E type
				unsigned int len = (found_it->second).size(); // get new count of cmb for one patient
				if (len > max_per_pt)
					max_per_pt = len;
			} else {
#ifdef ICD9_DEBUG_SETUP_TRACE
				std::cout << "new key " << vs[i] << "\n";
#endif
				VecStr vcodes;
				vcodes.reserve(approx_cmb_per_visit); // estimate of number of codes per patient.
				vcodes.push_back(s); // new vector of ICD codes with this first item
				visitCodes.insert(std::make_pair(vs[i],vcodes));
			} // end find
			lastVisitId = s;
			last_found_it = found_it;
		} //end match shortcut
	} // end loop through all visit-code input data
#ifdef ICD9_DEBUG_SETUP
	std::cout << "intermediate ragged-right map created\n";
#endif
	return raggedWideMultimapToMatrix(visitCodes, max_per_pt);
}


//' @title Convert long to wide from as matrix
//' @description Take a data frame with visits and ICD codes in two columns, and convert to a matrix with one row per visit.
//' Since multiple rows are combined when visits are out of sequence, no guarantee is made about the returned order. We sort implicitly.
//' For guaranteed order, we can't de-duplicate disordered visitIds, just aggregate contiguous blocks: icd9LongOrderedToWide does this quickly.
//' @export
// [[Rcpp::export]]
CharacterVector icd9LongToWideMatrix(const SEXP& icd9df, const std::string visitId="visitId", const std::string icd9Field="icd9") {
	SEXP icds = getListElement(icd9df, icd9Field.c_str());
	VecStr vs = as<VecStr>(
			as<CharacterVector>(getListElement(icd9df, visitId.c_str()))); // TODO can we do this in one step without Rcpp copying?
	const unsigned int approx_cmb_per_visit = 7; // just an estimate
	unsigned int vlen = Rf_length(icds);
	VecStr visitIds;
	visitIds.reserve(vlen / approx_cmb_per_visit);
	std::vector<VecStr> ragged; // intermediate structure
	unsigned int max_per_pt = 1;
	Str lastVisitId = "";
	for (unsigned int i = 0; i < vlen; ++i) {
		const char* s = CHAR(STRING_ELT(icds, i));
		if (lastVisitId != s || std::find(visitIds.rbegin(), visitIds.rend(), vs[i]) == visitIds.rend()) {
		//if (std::find(visitIds.rbegin(), visitIds.rend(), vs[i]) == visitIds.rend()) {
			VecStr vcodes;
			vcodes.reserve(approx_cmb_per_visit); // estimate of number of codes per patient.
			vcodes.push_back(s); // new vector of ICD codes with this first item
			ragged.push_back(vcodes); // and add that vector to the intermediate structure
			visitIds.push_back(vs[i]);
			//lastVisitId = s; // additional keep track of last one instead of having to find every time from whole list
		} else {
			ragged[ragged.size() - 1].push_back(s); // augment vec for current visit and N/V/E type
			unsigned int len = ragged[ragged.size() - 1].size(); // get new count of cmb for one patient
			if (len > max_per_pt)
				max_per_pt = len;
		}
	} // end loop through all visit-code input data

	return raggedWideVecVecToMatrix(ragged, max_per_pt, visitIds);
}


//' @title Convert ordered long to wide from as matrix
//' @description Take a data frame with visits and ICD codes in two columns, and convert to a matrix with one row per visit. Each time a new visitId is seen when scanning the list, a new row in the comorbidity output will be made. This will introduce >=1 extra row per out-of-order visitId, but not cause an error.
//' Since multiple rows are combined when visits are out of sequence, no guarantee is made about the returned order. We sort implicitly.
//' For guaranteed order, we can't de-duplicate disordered visitIds, just aggregate contiguous blocks: icd9LongOrderedToWide does this quickly.
//' @export
// [[Rcpp::export]]
SEXP icd9LongToWideMatrixOrdered(const SEXP& icd9df, const std::string visitId =
		"visitId", const std::string icd9Field = "icd9") {

	SEXP icds = getListElement(icd9df, icd9Field.c_str());
	VecStr vs = as<VecStr>(
			as<CharacterVector>(getListElement(icd9df, visitId.c_str()))); // TODO can we do this in one step without Rcpp copying?
	const unsigned int approx_cmb_per_visit = 7; // just an estimate
	unsigned int vlen = Rf_length(icds);
	VecStr visitIds;
	Str lastVisit = "";
	visitIds.reserve(vlen / approx_cmb_per_visit);
	std::vector<VecStr> ragged; // intermediate structure
	unsigned int max_per_pt = 1;
	Str last_visit;
	for (unsigned int i = 0; i < vlen; ++i) {
		const char* s = CHAR(STRING_ELT(icds, i));
		if (vs[i] != lastVisit) {
			VecStr vcodes;
			vcodes.reserve(approx_cmb_per_visit); // estimate of number of codes per patient.
			vcodes.push_back(s); // new vector of ICD codes with this first item
			ragged.push_back(vcodes); // and add that vector to the intermediate structure
			visitIds.push_back(vs[i]);
			lastVisit = vs[i];
		} else {
			int ragged_end = ragged.size()-1;
			ragged[ragged_end].push_back(s); // augment vec for current visit and N/V/E type
			unsigned int len = ragged[ragged_end].size(); // get new count of cmb for one patient
			if (len > max_per_pt)
				max_per_pt = len;
		}
	} // end loop through all visit-code input data
	return raggedWideVecVecToMatrix(ragged, max_per_pt, visitIds);
}
