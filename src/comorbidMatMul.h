// [[Rcpp::depends(RcppEigen)]]
#include "icd_types.h"
#include "local.h"
#include <algorithm>                   // for binary_search, copy
#include <vector>                      // for vector, vector<>::const_iterator
#include <unordered_set>
#include "refactor.h"
#include <string>
#include <cstring>
#include "relevant.h"
#include "mapplus.h"

using namespace Rcpp;

// # nocov start
#ifdef ICD_DEBUG
void printCornerMap(DenseMap x);
void printCornerSparse(PtsSparse x);
#define PRINTCORNERMAP(x) Rcpp::Rcout << #x << ": "; printCornerMap(x);
#define PRINTCORNERSP(x) Rcpp::Rcout << #x << ": "; printCornerSparse(x);
#define ICD_ASSIGN(row,col) mat(row, col) = true; // bounds check
#else
#define PRINTCORNERMAP(x) ((void)0);
#define PRINTCORNERSP(x) ((void)0);
#define ICD_ASSIGN(row,col) mat.coeffRef(row, col) = true;
#endif
// # nocov end

void buildVisitCodesSparseSimple(
    const RObject& visits,
    const RObject& codes, // todo handle factor in parent function
    Relevant& rh,
    PtsSparse& visMat, // output
    VecStr& visitIds // output: can get this from sparse matrix at end? Needed?
);
void buildVisitCodesSparseWide(
    const RObject& visits,
    const String id_field,
    const CV code_fields, // todo handle factor in parent function
    Relevant& rh,
    PtsSparse& visMat, // output
    VecStr& visitIds // output: can get this from sparse matrix at end? Needed?
);
LogicalMatrix comorbidMatMulWide(const DataFrame& data,
                                 const List& map,
                                 const std::string id_field,
                                 const CV code_fields);
LogicalMatrix comorbidMatMulSimple(const DataFrame& icd9df,
                                   const List& icd9Mapping,
                                   const std::string visitId,
                                   const std::string icd9Field);
