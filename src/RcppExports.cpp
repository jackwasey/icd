// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "icd_types.h"
#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// icd9MajMinToCode
CV icd9MajMinToCode(const CV mjr, const CV mnr, bool isShort);
RcppExport SEXP _icd_icd9MajMinToCode(SEXP mjrSEXP, SEXP mnrSEXP, SEXP isShortSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const CV >::type mjr(mjrSEXP);
    Rcpp::traits::input_parameter< const CV >::type mnr(mnrSEXP);
    Rcpp::traits::input_parameter< bool >::type isShort(isShortSEXP);
    rcpp_result_gen = Rcpp::wrap(icd9MajMinToCode(mjr, mnr, isShort));
    return rcpp_result_gen;
END_RCPP
}
// icd9MajMinToShort
CV icd9MajMinToShort(const CV mjr, const CV mnr);
RcppExport SEXP _icd_icd9MajMinToShort(SEXP mjrSEXP, SEXP mnrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const CV >::type mjr(mjrSEXP);
    Rcpp::traits::input_parameter< const CV >::type mnr(mnrSEXP);
    rcpp_result_gen = Rcpp::wrap(icd9MajMinToShort(mjr, mnr));
    return rcpp_result_gen;
END_RCPP
}
// icd9MajMinToDecimal
CV icd9MajMinToDecimal(const CV mjr, const CV mnr);
RcppExport SEXP _icd_icd9MajMinToDecimal(SEXP mjrSEXP, SEXP mnrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const CV >::type mjr(mjrSEXP);
    Rcpp::traits::input_parameter< const CV >::type mnr(mnrSEXP);
    rcpp_result_gen = Rcpp::wrap(icd9MajMinToDecimal(mjr, mnr));
    return rcpp_result_gen;
END_RCPP
}
// setDecimalDiag
void setDecimalDiag(RObject& x, bool value);
RcppExport SEXP _icd_setDecimalDiag(SEXP xSEXP, SEXP valueSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< RObject& >::type x(xSEXP);
    Rcpp::traits::input_parameter< bool >::type value(valueSEXP);
    setDecimalDiag(x, value);
    return R_NilValue;
END_RCPP
}
// setShortDiag
void setShortDiag(RObject& x, bool value);
RcppExport SEXP _icd_setShortDiag(SEXP xSEXP, SEXP valueSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< RObject& >::type x(xSEXP);
    Rcpp::traits::input_parameter< bool >::type value(valueSEXP);
    setShortDiag(x, value);
    return R_NilValue;
END_RCPP
}
// categorize_rcpp
List categorize_rcpp();
RcppExport SEXP _icd_categorize_rcpp() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(categorize_rcpp());
    return rcpp_result_gen;
END_RCPP
}
// icd10ChildrenDefined
CV icd10ChildrenDefined(CV& x, List lookup, IntegerVector nc);
RcppExport SEXP _icd_icd10ChildrenDefined(SEXP xSEXP, SEXP lookupSEXP, SEXP ncSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CV& >::type x(xSEXP);
    Rcpp::traits::input_parameter< List >::type lookup(lookupSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type nc(ncSEXP);
    rcpp_result_gen = Rcpp::wrap(icd10ChildrenDefined(x, lookup, nc));
    return rcpp_result_gen;
END_RCPP
}
// simplifyMapLexicographic
Rcpp::List simplifyMapLexicographic(const CV& pt_codes, const List map);
RcppExport SEXP _icd_simplifyMapLexicographic(SEXP pt_codesSEXP, SEXP mapSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const CV& >::type pt_codes(pt_codesSEXP);
    Rcpp::traits::input_parameter< const List >::type map(mapSEXP);
    rcpp_result_gen = Rcpp::wrap(simplifyMapLexicographic(pt_codes, map));
    return rcpp_result_gen;
END_RCPP
}
// comorbidMatMulWide
LogicalMatrix comorbidMatMulWide(const DataFrame& data, const List& map, const std::string id_name, const CV code_names, const bool validate);
RcppExport SEXP _icd_comorbidMatMulWide(SEXP dataSEXP, SEXP mapSEXP, SEXP id_nameSEXP, SEXP code_namesSEXP, SEXP validateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const DataFrame& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< const List& >::type map(mapSEXP);
    Rcpp::traits::input_parameter< const std::string >::type id_name(id_nameSEXP);
    Rcpp::traits::input_parameter< const CV >::type code_names(code_namesSEXP);
    Rcpp::traits::input_parameter< const bool >::type validate(validateSEXP);
    rcpp_result_gen = Rcpp::wrap(comorbidMatMulWide(data, map, id_name, code_names, validate));
    return rcpp_result_gen;
END_RCPP
}
// icd9PartsToShort
CV icd9PartsToShort(const List& parts);
RcppExport SEXP _icd_icd9PartsToShort(SEXP partsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List& >::type parts(partsSEXP);
    rcpp_result_gen = Rcpp::wrap(icd9PartsToShort(parts));
    return rcpp_result_gen;
END_RCPP
}
// icd9PartsToDecimal
CV icd9PartsToDecimal(const List& parts);
RcppExport SEXP _icd_icd9PartsToDecimal(SEXP partsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List& >::type parts(partsSEXP);
    rcpp_result_gen = Rcpp::wrap(icd9PartsToDecimal(parts));
    return rcpp_result_gen;
END_RCPP
}
// majMinToParts
List majMinToParts(const CV& mjr, const CV& mnr);
RcppExport SEXP _icd_majMinToParts(SEXP mjrSEXP, SEXP mnrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const CV& >::type mjr(mjrSEXP);
    Rcpp::traits::input_parameter< const CV& >::type mnr(mnrSEXP);
    rcpp_result_gen = Rcpp::wrap(majMinToParts(mjr, mnr));
    return rcpp_result_gen;
END_RCPP
}
// icd9ShortToParts
List icd9ShortToParts(const CV& icd9Short, String mnrEmpty);
RcppExport SEXP _icd_icd9ShortToParts(SEXP icd9ShortSEXP, SEXP mnrEmptySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const CV& >::type icd9Short(icd9ShortSEXP);
    Rcpp::traits::input_parameter< String >::type mnrEmpty(mnrEmptySEXP);
    rcpp_result_gen = Rcpp::wrap(icd9ShortToParts(icd9Short, mnrEmpty));
    return rcpp_result_gen;
END_RCPP
}
// icd9DecimalToParts
List icd9DecimalToParts(const CV& icd9Decimal, const String mnrEmpty);
RcppExport SEXP _icd_icd9DecimalToParts(SEXP icd9DecimalSEXP, SEXP mnrEmptySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const CV& >::type icd9Decimal(icd9DecimalSEXP);
    Rcpp::traits::input_parameter< const String >::type mnrEmpty(mnrEmptySEXP);
    rcpp_result_gen = Rcpp::wrap(icd9DecimalToParts(icd9Decimal, mnrEmpty));
    return rcpp_result_gen;
END_RCPP
}
// icd9ShortToDecimal
CV icd9ShortToDecimal(const CV& x);
RcppExport SEXP _icd_icd9ShortToDecimal(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const CV& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(icd9ShortToDecimal(x));
    return rcpp_result_gen;
END_RCPP
}
// icd9DecimalToShort
CV icd9DecimalToShort(const CV& x);
RcppExport SEXP _icd_icd9DecimalToShort(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const CV& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(icd9DecimalToShort(x));
    return rcpp_result_gen;
END_RCPP
}
// icd9GetMajor
CV icd9GetMajor(const CV& x, const bool short_code);
RcppExport SEXP _icd_icd9GetMajor(SEXP xSEXP, SEXP short_codeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const CV& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const bool >::type short_code(short_codeSEXP);
    rcpp_result_gen = Rcpp::wrap(icd9GetMajor(x, short_code));
    return rcpp_result_gen;
END_RCPP
}
// icd10ShortToParts
List icd10ShortToParts(const CV& x, const String mnrEmpty);
RcppExport SEXP _icd_icd10ShortToParts(SEXP xSEXP, SEXP mnrEmptySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const CV& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const String >::type mnrEmpty(mnrEmptySEXP);
    rcpp_result_gen = Rcpp::wrap(icd10ShortToParts(x, mnrEmpty));
    return rcpp_result_gen;
END_RCPP
}
// icd10DecimalToParts
List icd10DecimalToParts(const CV x, const String mnrEmpty);
RcppExport SEXP _icd_icd10DecimalToParts(SEXP xSEXP, SEXP mnrEmptySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const CV >::type x(xSEXP);
    Rcpp::traits::input_parameter< const String >::type mnrEmpty(mnrEmptySEXP);
    rcpp_result_gen = Rcpp::wrap(icd10DecimalToParts(x, mnrEmpty));
    return rcpp_result_gen;
END_RCPP
}
// fastIntToStringRcpp
CharacterVector fastIntToStringRcpp(IntegerVector x);
RcppExport SEXP _icd_fastIntToStringRcpp(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(fastIntToStringRcpp(x));
    return rcpp_result_gen;
END_RCPP
}
// guessShortPlusFactorCpp
bool guessShortPlusFactorCpp(SEXP x_, int n);
RcppExport SEXP _icd_guessShortPlusFactorCpp(SEXP x_SEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x_(x_SEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(guessShortPlusFactorCpp(x_, n));
    return rcpp_result_gen;
END_RCPP
}
// guessShortCompleteCpp
bool guessShortCompleteCpp(SEXP x_, SEXP short_code, int n, SEXP icd_name);
RcppExport SEXP _icd_guessShortCompleteCpp(SEXP x_SEXP, SEXP short_codeSEXP, SEXP nSEXP, SEXP icd_nameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x_(x_SEXP);
    Rcpp::traits::input_parameter< SEXP >::type short_code(short_codeSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< SEXP >::type icd_name(icd_nameSEXP);
    rcpp_result_gen = Rcpp::wrap(guessShortCompleteCpp(x_, short_code, n, icd_name));
    return rcpp_result_gen;
END_RCPP
}
// icd9_is_n_rcpp
std::vector<bool> icd9_is_n_rcpp(const VecStr& sv);
RcppExport SEXP _icd_icd9_is_n_rcpp(SEXP svSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const VecStr& >::type sv(svSEXP);
    rcpp_result_gen = Rcpp::wrap(icd9_is_n_rcpp(sv));
    return rcpp_result_gen;
END_RCPP
}
// icd9_is_v_rcpp
std::vector<bool> icd9_is_v_rcpp(const VecStr& sv);
RcppExport SEXP _icd_icd9_is_v_rcpp(SEXP svSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const VecStr& >::type sv(svSEXP);
    rcpp_result_gen = Rcpp::wrap(icd9_is_v_rcpp(sv));
    return rcpp_result_gen;
END_RCPP
}
// icd9_is_e_rcpp
std::vector<bool> icd9_is_e_rcpp(const VecStr& sv);
RcppExport SEXP _icd_icd9_is_e_rcpp(SEXP svSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const VecStr& >::type sv(svSEXP);
    rcpp_result_gen = Rcpp::wrap(icd9_is_e_rcpp(sv));
    return rcpp_result_gen;
END_RCPP
}
// icd9AddLeadingZeroesMajorSingle
String icd9AddLeadingZeroesMajorSingle(String mjr);
RcppExport SEXP _icd_icd9AddLeadingZeroesMajorSingle(SEXP mjrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< String >::type mjr(mjrSEXP);
    rcpp_result_gen = Rcpp::wrap(icd9AddLeadingZeroesMajorSingle(mjr));
    return rcpp_result_gen;
END_RCPP
}
// icd9AddLeadingZeroesMajorSingleStd
std::string icd9AddLeadingZeroesMajorSingleStd(std::string m);
RcppExport SEXP _icd_icd9AddLeadingZeroesMajorSingleStd(SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(icd9AddLeadingZeroesMajorSingleStd(m));
    return rcpp_result_gen;
END_RCPP
}
// icd9AddLeadingZeroesMajor
CV icd9AddLeadingZeroesMajor(CV mjr);
RcppExport SEXP _icd_icd9AddLeadingZeroesMajor(SEXP mjrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CV >::type mjr(mjrSEXP);
    rcpp_result_gen = Rcpp::wrap(icd9AddLeadingZeroesMajor(mjr));
    return rcpp_result_gen;
END_RCPP
}
// icd9AddLeadingZeroes
CV icd9AddLeadingZeroes(CV x, bool short_code);
RcppExport SEXP _icd_icd9AddLeadingZeroes(SEXP xSEXP, SEXP short_codeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CV >::type x(xSEXP);
    Rcpp::traits::input_parameter< bool >::type short_code(short_codeSEXP);
    rcpp_result_gen = Rcpp::wrap(icd9AddLeadingZeroes(x, short_code));
    return rcpp_result_gen;
END_RCPP
}
// icd9ExpandMinor
CV icd9ExpandMinor(const Str& mnr, bool isE);
RcppExport SEXP _icd_icd9ExpandMinor(SEXP mnrSEXP, SEXP isESEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Str& >::type mnr(mnrSEXP);
    Rcpp::traits::input_parameter< bool >::type isE(isESEXP);
    rcpp_result_gen = Rcpp::wrap(icd9ExpandMinor(mnr, isE));
    return rcpp_result_gen;
END_RCPP
}
// icd9ChildrenShort
CV icd9ChildrenShort(CV icd9Short, const VecStr& icd9cmReal, bool onlyReal);
RcppExport SEXP _icd_icd9ChildrenShort(SEXP icd9ShortSEXP, SEXP icd9cmRealSEXP, SEXP onlyRealSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CV >::type icd9Short(icd9ShortSEXP);
    Rcpp::traits::input_parameter< const VecStr& >::type icd9cmReal(icd9cmRealSEXP);
    Rcpp::traits::input_parameter< bool >::type onlyReal(onlyRealSEXP);
    rcpp_result_gen = Rcpp::wrap(icd9ChildrenShort(icd9Short, icd9cmReal, onlyReal));
    return rcpp_result_gen;
END_RCPP
}
// icd9ChildrenShortUnordered
CV icd9ChildrenShortUnordered(const CV& icd9Short, const VecStr& icd9cmReal, const bool onlyReal);
RcppExport SEXP _icd_icd9ChildrenShortUnordered(SEXP icd9ShortSEXP, SEXP icd9cmRealSEXP, SEXP onlyRealSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const CV& >::type icd9Short(icd9ShortSEXP);
    Rcpp::traits::input_parameter< const VecStr& >::type icd9cmReal(icd9cmRealSEXP);
    Rcpp::traits::input_parameter< const bool >::type onlyReal(onlyRealSEXP);
    rcpp_result_gen = Rcpp::wrap(icd9ChildrenShortUnordered(icd9Short, icd9cmReal, onlyReal));
    return rcpp_result_gen;
END_RCPP
}
// icd9ChildrenDecimalCpp
CV icd9ChildrenDecimalCpp(CV icd9Decimal, const VecStr& icd9cmReal, bool onlyReal);
RcppExport SEXP _icd_icd9ChildrenDecimalCpp(SEXP icd9DecimalSEXP, SEXP icd9cmRealSEXP, SEXP onlyRealSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CV >::type icd9Decimal(icd9DecimalSEXP);
    Rcpp::traits::input_parameter< const VecStr& >::type icd9cmReal(icd9cmRealSEXP);
    Rcpp::traits::input_parameter< bool >::type onlyReal(onlyRealSEXP);
    rcpp_result_gen = Rcpp::wrap(icd9ChildrenDecimalCpp(icd9Decimal, icd9cmReal, onlyReal));
    return rcpp_result_gen;
END_RCPP
}
// icd9ChildrenCpp
CV icd9ChildrenCpp(CV icd9, bool isShort, const VecStr icd9cmReal, bool onlyReal);
RcppExport SEXP _icd_icd9ChildrenCpp(SEXP icd9SEXP, SEXP isShortSEXP, SEXP icd9cmRealSEXP, SEXP onlyRealSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CV >::type icd9(icd9SEXP);
    Rcpp::traits::input_parameter< bool >::type isShort(isShortSEXP);
    Rcpp::traits::input_parameter< const VecStr >::type icd9cmReal(icd9cmRealSEXP);
    Rcpp::traits::input_parameter< bool >::type onlyReal(onlyRealSEXP);
    rcpp_result_gen = Rcpp::wrap(icd9ChildrenCpp(icd9, isShort, icd9cmReal, onlyReal));
    return rcpp_result_gen;
END_RCPP
}
// factorNoSort
IntegerVector factorNoSort(const CharacterVector& x, const CharacterVector& levels, const bool na_rm);
RcppExport SEXP _icd_factorNoSort(SEXP xSEXP, SEXP levelsSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const CharacterVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const CharacterVector& >::type levels(levelsSEXP);
    Rcpp::traits::input_parameter< const bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(factorNoSort(x, levels, na_rm));
    return rcpp_result_gen;
END_RCPP
}
// refactor
IntegerVector refactor(const IntegerVector& x, const CV& new_levels, const bool exclude_na, const bool validate);
RcppExport SEXP _icd_refactor(SEXP xSEXP, SEXP new_levelsSEXP, SEXP exclude_naSEXP, SEXP validateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const CV& >::type new_levels(new_levelsSEXP);
    Rcpp::traits::input_parameter< const bool >::type exclude_na(exclude_naSEXP);
    Rcpp::traits::input_parameter< const bool >::type validate(validateSEXP);
    rcpp_result_gen = Rcpp::wrap(refactor(x, new_levels, exclude_na, validate));
    return rcpp_result_gen;
END_RCPP
}
// refactor_narm
IntegerVector refactor_narm(const IntegerVector& x, const CV& new_levels, const bool validate);
RcppExport SEXP _icd_refactor_narm(SEXP xSEXP, SEXP new_levelsSEXP, SEXP validateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const CV& >::type new_levels(new_levelsSEXP);
    Rcpp::traits::input_parameter< const bool >::type validate(validateSEXP);
    rcpp_result_gen = Rcpp::wrap(refactor_narm(x, new_levels, validate));
    return rcpp_result_gen;
END_RCPP
}
// factorIsValid
bool factorIsValid(const IntegerVector& f);
RcppExport SEXP _icd_factorIsValid(SEXP fSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerVector& >::type f(fSEXP);
    rcpp_result_gen = Rcpp::wrap(factorIsValid(f));
    return rcpp_result_gen;
END_RCPP
}
// trimLeftCpp
std::string trimLeftCpp(std::string s);
RcppExport SEXP _icd_trimLeftCpp(SEXP sSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type s(sSEXP);
    rcpp_result_gen = Rcpp::wrap(trimLeftCpp(s));
    return rcpp_result_gen;
END_RCPP
}
// strimCpp
std::string strimCpp(std::string s);
RcppExport SEXP _icd_strimCpp(SEXP sSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type s(sSEXP);
    rcpp_result_gen = Rcpp::wrap(strimCpp(s));
    return rcpp_result_gen;
END_RCPP
}
// trimCpp
VecStr trimCpp(VecStr sv);
RcppExport SEXP _icd_trimCpp(SEXP svSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< VecStr >::type sv(svSEXP);
    rcpp_result_gen = Rcpp::wrap(trimCpp(sv));
    return rcpp_result_gen;
END_RCPP
}
// icd9Compare
bool icd9Compare(std::string a, std::string b);
RcppExport SEXP _icd_icd9Compare(SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type a(aSEXP);
    Rcpp::traits::input_parameter< std::string >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(icd9Compare(a, b));
    return rcpp_result_gen;
END_RCPP
}
// icd9Order
std::vector<std::size_t> icd9Order(VecStr x);
RcppExport SEXP _icd_icd9Order(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< VecStr >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(icd9Order(x));
    return rcpp_result_gen;
END_RCPP
}
// icd10cmCompare
bool icd10cmCompare(const Rcpp::String x, const Rcpp::String y);
RcppExport SEXP _icd_icd10cmCompare(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::String >::type x(xSEXP);
    Rcpp::traits::input_parameter< const Rcpp::String >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(icd10cmCompare(x, y));
    return rcpp_result_gen;
END_RCPP
}
// icd10cmSort
CharacterVector icd10cmSort(const CharacterVector& x);
RcppExport SEXP _icd_icd10cmSort(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const CharacterVector& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(icd10cmSort(x));
    return rcpp_result_gen;
END_RCPP
}
// icd10cmOrder
IntegerVector icd10cmOrder(const CharacterVector& x);
RcppExport SEXP _icd_icd10cmOrder(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const CharacterVector& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(icd10cmOrder(x));
    return rcpp_result_gen;
END_RCPP
}
// matchFast
SEXP matchFast(SEXP x, SEXP table);
RcppExport SEXP _icd_matchFast(SEXP xSEXP, SEXP tableSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type table(tableSEXP);
    rcpp_result_gen = Rcpp::wrap(matchFast(x, table));
    return rcpp_result_gen;
END_RCPP
}
// inFast
SEXP inFast(SEXP x, SEXP table);
RcppExport SEXP _icd_inFast(SEXP xSEXP, SEXP tableSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type table(tableSEXP);
    rcpp_result_gen = Rcpp::wrap(inFast(x, table));
    return rcpp_result_gen;
END_RCPP
}
// valgrindCallgrindStart
int valgrindCallgrindStart(bool zerostats);
RcppExport SEXP _icd_valgrindCallgrindStart(SEXP zerostatsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< bool >::type zerostats(zerostatsSEXP);
    rcpp_result_gen = Rcpp::wrap(valgrindCallgrindStart(zerostats));
    return rcpp_result_gen;
END_RCPP
}
// valgrindCallgrindStop
int valgrindCallgrindStop();
RcppExport SEXP _icd_valgrindCallgrindStop() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(valgrindCallgrindStop());
    return rcpp_result_gen;
END_RCPP
}

RcppExport SEXP run_testthat_tests();

static const R_CallMethodDef CallEntries[] = {
    {"_icd_icd9MajMinToCode", (DL_FUNC) &_icd_icd9MajMinToCode, 3},
    {"_icd_icd9MajMinToShort", (DL_FUNC) &_icd_icd9MajMinToShort, 2},
    {"_icd_icd9MajMinToDecimal", (DL_FUNC) &_icd_icd9MajMinToDecimal, 2},
    {"_icd_setDecimalDiag", (DL_FUNC) &_icd_setDecimalDiag, 2},
    {"_icd_setShortDiag", (DL_FUNC) &_icd_setShortDiag, 2},
    {"_icd_categorize_rcpp", (DL_FUNC) &_icd_categorize_rcpp, 0},
    {"_icd_icd10ChildrenDefined", (DL_FUNC) &_icd_icd10ChildrenDefined, 3},
    {"_icd_simplifyMapLexicographic", (DL_FUNC) &_icd_simplifyMapLexicographic, 2},
    {"_icd_comorbidMatMulWide", (DL_FUNC) &_icd_comorbidMatMulWide, 5},
    {"_icd_icd9PartsToShort", (DL_FUNC) &_icd_icd9PartsToShort, 1},
    {"_icd_icd9PartsToDecimal", (DL_FUNC) &_icd_icd9PartsToDecimal, 1},
    {"_icd_majMinToParts", (DL_FUNC) &_icd_majMinToParts, 2},
    {"_icd_icd9ShortToParts", (DL_FUNC) &_icd_icd9ShortToParts, 2},
    {"_icd_icd9DecimalToParts", (DL_FUNC) &_icd_icd9DecimalToParts, 2},
    {"_icd_icd9ShortToDecimal", (DL_FUNC) &_icd_icd9ShortToDecimal, 1},
    {"_icd_icd9DecimalToShort", (DL_FUNC) &_icd_icd9DecimalToShort, 1},
    {"_icd_icd9GetMajor", (DL_FUNC) &_icd_icd9GetMajor, 2},
    {"_icd_icd10ShortToParts", (DL_FUNC) &_icd_icd10ShortToParts, 2},
    {"_icd_icd10DecimalToParts", (DL_FUNC) &_icd_icd10DecimalToParts, 2},
    {"_icd_fastIntToStringRcpp", (DL_FUNC) &_icd_fastIntToStringRcpp, 1},
    {"_icd_guessShortPlusFactorCpp", (DL_FUNC) &_icd_guessShortPlusFactorCpp, 2},
    {"_icd_guessShortCompleteCpp", (DL_FUNC) &_icd_guessShortCompleteCpp, 4},
    {"_icd_icd9_is_n_rcpp", (DL_FUNC) &_icd_icd9_is_n_rcpp, 1},
    {"_icd_icd9_is_v_rcpp", (DL_FUNC) &_icd_icd9_is_v_rcpp, 1},
    {"_icd_icd9_is_e_rcpp", (DL_FUNC) &_icd_icd9_is_e_rcpp, 1},
    {"_icd_icd9AddLeadingZeroesMajorSingle", (DL_FUNC) &_icd_icd9AddLeadingZeroesMajorSingle, 1},
    {"_icd_icd9AddLeadingZeroesMajorSingleStd", (DL_FUNC) &_icd_icd9AddLeadingZeroesMajorSingleStd, 1},
    {"_icd_icd9AddLeadingZeroesMajor", (DL_FUNC) &_icd_icd9AddLeadingZeroesMajor, 1},
    {"_icd_icd9AddLeadingZeroes", (DL_FUNC) &_icd_icd9AddLeadingZeroes, 2},
    {"_icd_icd9ExpandMinor", (DL_FUNC) &_icd_icd9ExpandMinor, 2},
    {"_icd_icd9ChildrenShort", (DL_FUNC) &_icd_icd9ChildrenShort, 3},
    {"_icd_icd9ChildrenShortUnordered", (DL_FUNC) &_icd_icd9ChildrenShortUnordered, 3},
    {"_icd_icd9ChildrenDecimalCpp", (DL_FUNC) &_icd_icd9ChildrenDecimalCpp, 3},
    {"_icd_icd9ChildrenCpp", (DL_FUNC) &_icd_icd9ChildrenCpp, 4},
    {"_icd_factorNoSort", (DL_FUNC) &_icd_factorNoSort, 3},
    {"_icd_refactor", (DL_FUNC) &_icd_refactor, 4},
    {"_icd_refactor_narm", (DL_FUNC) &_icd_refactor_narm, 3},
    {"_icd_factorIsValid", (DL_FUNC) &_icd_factorIsValid, 1},
    {"_icd_trimLeftCpp", (DL_FUNC) &_icd_trimLeftCpp, 1},
    {"_icd_strimCpp", (DL_FUNC) &_icd_strimCpp, 1},
    {"_icd_trimCpp", (DL_FUNC) &_icd_trimCpp, 1},
    {"_icd_icd9Compare", (DL_FUNC) &_icd_icd9Compare, 2},
    {"_icd_icd9Order", (DL_FUNC) &_icd_icd9Order, 1},
    {"_icd_icd10cmCompare", (DL_FUNC) &_icd_icd10cmCompare, 2},
    {"_icd_icd10cmSort", (DL_FUNC) &_icd_icd10cmSort, 1},
    {"_icd_icd10cmOrder", (DL_FUNC) &_icd_icd10cmOrder, 1},
    {"_icd_matchFast", (DL_FUNC) &_icd_matchFast, 2},
    {"_icd_inFast", (DL_FUNC) &_icd_inFast, 2},
    {"_icd_valgrindCallgrindStart", (DL_FUNC) &_icd_valgrindCallgrindStart, 1},
    {"_icd_valgrindCallgrindStop", (DL_FUNC) &_icd_valgrindCallgrindStop, 0},
    {"run_testthat_tests",                      (DL_FUNC) &run_testthat_tests,                      0},
    {NULL, NULL, 0}
};

RcppExport void R_init_icd(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
