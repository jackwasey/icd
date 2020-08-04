#ifndef SORT_H_
#define SORT_H_

#include <Rcpp.h>
#include <algorithm> // std::sort
#include <functional>
#include <string.h>
#include <utility> // std::pair

typedef std::pair<std::string, std::size_t> pas_std;
typedef std::pair<Rcpp::String, R_xlen_t> pas_rcpp;
bool icd9Compare(Rcpp::String a, Rcpp::String b);
bool icd9CompareStd(std::string a, std::string b);

Rcpp::CharacterVector icd9Sort(const Rcpp::CharacterVector& x);

// or IntegerVector?
std::vector<std::size_t> icd9OrderStd(const std::vector<std::string>& x);

bool icd10cmCompareQuirk(const char* x,
                         const char* y,
                         const char* quirk,
                         const char* beforeQuirk,
                         const char* afterQuirk,
                         const char* beforeBeforeQuirk,
                         const char* afterAfterQuirk,
                         bool& res);
// inline in cpp: bool icd10cmCompare(const String& x, const String& y);
Rcpp::CharacterVector icd10cmSort(const Rcpp::CharacterVector& x);
// disable until std and rcpp implemented fully VecStr icd10cmSortStd(const
// std::vector<std::string> &x);
// Rcpp::IntegerVector icd10cmOrder(const Rcpp::CharacterVector &x);
// std::vector<std::size_t> icd10cmOrderStd(const std::vector<std::string> &x);
#endif /* SORT_H_ */
