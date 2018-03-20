#include <Rcpp.h>
#include <RcppEigen.h>
#include <Eigen/SparseCore>
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::interfaces(r, cpp)]]
#include "comorbidCommon.h"
#include "comorbidSetup.h"
#include <Rcpp.h>
#include <algorithm>                   // for binary_search, copy
#include <vector>                      // for vector, vector<>::const_iterator
#include "Rcpp/iostream/Rstreambuf.h"  // for Rcout
#include "icd_types.h"                 // for ComorbidOut, VecVecInt, VecVec...
#include "local.h"                     // for ICD_OPENMP
#include "config.h"                     // for valgrind, CXX11 etc
#include "util.h"                     // for debug_parallel

using namespace Rcpp;

// use row-major sparse matrix - row major because easier to insert into Eigen
// sparse matrix, and we discover comorbidities one patient at a time, i.e. row
// major

// using the typedef confuses Rcpp
//typedef Eigen::SparseMatrix<char, Eigen::RowMajor> SparseOut; // bool, char or int?
// https://eigen.tuxfamily.org/dox/group__TutorialSparse.html
typedef Eigen::Triplet<char> Triplet;

//' comorbidity search with sparse matrix result
//'
//' Much less memory competition in writing output. As an example the Vermont
//' data has 29,000 comorbidity flags (29 for each patient) Whereas only 2367
//' AHRQ comorbidity are positive, so under 10%.
//' @keywords internal
// [[Rcpp::export]]
Eigen::SparseMatrix<char, Eigen::RowMajor> lookupComorbidSparse(const VecVecInt& vcdb, const VecVecInt& map) {
  const VecVecIntSz num_comorbid = map.size();
  VecVecIntSz vis_i;
  VecInt::const_iterator code_it;
  std::vector<Triplet> vecTrip;
  vecTrip.reserve(vcdb.size());
  for (vis_i = 0; vis_i < vcdb.size(); ++vis_i) {
    const VecInt& codes = vcdb[vis_i]; // these are the ICD-9 codes for the current visitid
    const VecIntIt cbegin = codes.begin();
    const VecIntIt cend = codes.end();
    for (code_it = cbegin; code_it != cend; ++code_it) {
      for (NewOutPt::size_type cmb = 0; cmb != num_comorbid; ++cmb) {
        const VecInt& mapCodes = map[cmb]; // may be zero length
        bool found_it = std::binary_search(mapCodes.begin(), mapCodes.end(), *code_it);
        if (found_it) {
          vecTrip.push_back(Triplet(vis_i, cmb, true));
          break;
        } // end if found_it
      } // end loop through comorbidities
    } // end loop through all ICD codes for one patient
  } // end main loop through patient visits
  Eigen::SparseMatrix<char, Eigen::RowMajor> out(vcdb.size(), num_comorbid);
  out.setFromTriplets(vecTrip.begin(), vecTrip.end());
  return out;
  }

Eigen::SparseMatrix<char, Eigen::RowMajor> lookupComorbidSparseOmp(const VecVecInt& vcdb, const VecVecInt& map) {
  const VecVecIntSz num_comorbid = map.size();
  VecInt::const_iterator code_it;
  std::vector<Triplet> vecTrip;
  vecTrip.reserve(vcdb.size());
#pragma omp taskloop shared(Rcpp::Rcout, out) //grainsize (256)
  for (VecVecIntSz vis_i = 0; vis_i < vcdb.size(); ++vis_i) {
    const VecInt& codes = vcdb[vis_i]; // these are the ICD-9 codes for the current visitid
    const VecIntIt cbegin = codes.begin();
    const VecIntIt cend = codes.end();
    for (code_it = cbegin; code_it != cend; ++code_it) {
      for (NewOutPt::size_type cmb = 0; cmb != num_comorbid; ++cmb) {
        const VecInt& mapCodes = map[cmb]; // may be zero length
        bool found_it = std::binary_search(mapCodes.begin(), mapCodes.end(), *code_it);
        if (found_it) {
          vecTrip.push_back(Triplet(vis_i, cmb, true));
          break;
        } // end if found_it
      } // end loop through comorbidities
    } // end loop through all ICD codes for one patient
  } // end main loop through patient visits
  Eigen::SparseMatrix<char, Eigen::RowMajor> out(vcdb.size(), num_comorbid);
  out.setFromTriplets(vecTrip.begin(), vecTrip.end());
  return out;
}

//' @describeIn icd9ComorbidTaskloop Sparse comorbidity results with Eigen
//' @keywords internal
// [[Rcpp::export]]
SEXP icd9ComorbidSparse(const SEXP& icd9df, const Rcpp::List& icd9Mapping,
                        const std::string visitId, const std::string icd9Field,
                        const int threads = 8, const int chunk_size = 256,
                        const int omp_chunk_size = 1, bool aggregate = true) {

  valgrindCallgrindStart(false);
  VecStr out_row_names; // size is reserved in buildVisitCodesVec
  VecVecInt vcdb; // size is reserved later

  const SEXP vsexp = PROTECT(getRListOrDfElement(icd9df, visitId.c_str()));
  if (TYPEOF(vsexp) != STRSXP) {
    Rcpp::stop("expecting visit ID in input data frame to be character vector");
    UNPROTECT(1); // vsexp
  }
  UNPROTECT(1); // vsexp not used further
  buildVisitCodesVec(icd9df, visitId, icd9Field, vcdb, out_row_names, aggregate);

  VecVecInt map;
  buildMap(icd9Mapping, map);

  const VecVecIntSz num_comorbid = map.size();
  const VecVecIntSz num_visits = vcdb.size();

  Eigen::SparseMatrix<char, Eigen::RowMajor> out = lookupComorbidSparse(vcdb, map);
  out.transpose();

  //Rcpp::IntegerMatrix mat_out = Rcpp::wrap(out);
  //mat_out.attr("dim") = Rcpp::Dimension((int) num_comorbid, (int) num_visits);
  //mat_out.attr("dimnames") = Rcpp::List::create(icd9Mapping.names(), out_row_names);
  valgrindCallgrindStop();
  //return mat_out;
  return Rcpp::wrap(out);
}

template <typename T>
T my_len( const T& x){
  return x.length() ;
}

//' @title prototype to do entire comorbidity calculation as a matrix multiplication
//' @description
//' The problem is that the matrices could be huge: the patient-icd matrix would
//' be millions of patient rows, and ~15000 columns for all AHRQ comorbidities.
//'
//' Several ways of reducing the problem: firstly, as with existing code, we can
//' drop any ICD codes from the map which are not in the patient data. With many
//' patients, this will be less effective as the long tail becomes apparent.
//' However, with the (small) Vermont data, we see ~15,000 codes being reduced to
//' 339.
//'
//' \section{Sparse matrices} Using sparse matrices is another solution. Building
//' the initial matrix may become a significant part of the calculation, but once
//' done, the solution could be a simple matrix multiplication, which is
//' potentially highly optimized (Eigen, BLAS, GPU, etc.)
//'
//' \section{Eigen} Eigen has parallel (non-GPU) optimized sparse row-major *
//' dense matrix. Patients-ICD matrix must be the row-major sparse one, so the
//' dense matrix is then the comorbidity map
//'
//' @examples
//' # show how many discrete ICD codes there are in the AHRQ map, before reducing
//' # to the number which actually appear in a group of patient visits
//' sapply(icd::icd9_map_ahrq, length) %>% sum
//' icd_comorbid_ahrq(vermont_dx %>% icd_wide_to_long, comorbid_fun = icd:::icd9ComorbidMatMul)
//' @keywords internal
// [[Rcpp::export]]
SEXP icd9ComorbidMatMul(const SEXP& icd9df, const Rcpp::List& icd9Mapping,
                        const std::string visitId, const std::string icd9Field,
                        const int threads = 8, const int chunk_size = 256,
                        const int omp_chunk_size = 1, bool aggregate = true) {

  // find size of final map matrix:
  size_t map_rows = 0;
  for (List::const_iterator li = icd9Mapping.begin(); li != icd9Mapping.end(); ++li) {
    IntegerVector v = *li;
    map_rows += v.size();
  }

  size_t row = 0;
  Eigen::MatrixXi map(map_rows, icd9Mapping.length());
  for (List::const_iterator li = icd9Mapping.begin(); li != icd9Mapping.end(); ++li) {
    IntegerVector v = *li;
    for (IntegerVector::iterator vi = v.begin(); vi != v.end(); ++vi, ++row) {
      map.coeffRef(row, std::distance(v.begin(), vi)) = true;
    }
  }
  Rcpp::Rcout << "first cell of map should be 1: it is " << map(0, 0) << "\n";

  // now build the patient:icd matrix... can probably re-use and simplify the
  // function 'buildVisitCodesVec'

  // Eigen::MatrixXi map(map_rows,
  //icd9Mapping.length());

  return List();
}
//' @describeIn icd9ComorbidTaskloop Sparse comorbidity results with Eigen
//' @keywords internal
// [[Rcpp::export]]
SEXP icd9ComorbidSparseOmp(const SEXP& icd9df, const Rcpp::List& icd9Mapping,
                           const std::string visitId, const std::string icd9Field,
                           const int threads = 8, const int chunk_size = 256,
                           const int omp_chunk_size = 1, bool aggregate = true) {

  valgrindCallgrindStart(false);
  VecStr out_row_names; // size is reserved in buildVisitCodesVec
  VecVecInt vcdb; // size is reserved later

  const SEXP vsexp = PROTECT(getRListOrDfElement(icd9df, visitId.c_str()));
  if (TYPEOF(vsexp) != STRSXP) {
    Rcpp::stop("expecting visit ID in input data frame to be character vector");
    UNPROTECT(1); // vsexp
  }
  UNPROTECT(1); // vsexp not used further
  buildVisitCodesVec(icd9df, visitId, icd9Field, vcdb, out_row_names, aggregate);

  VecVecInt map;
  buildMap(icd9Mapping, map);

  const VecVecIntSz num_comorbid = map.size();
  const VecVecIntSz num_visits = vcdb.size();

  Eigen::SparseMatrix<char, Eigen::RowMajor> out = lookupComorbidSparseOmp(vcdb, map);
  out.transpose();

  //Rcpp::IntegerMatrix mat_out = Rcpp::wrap(out);
  //mat_out.attr("dim") = Rcpp::Dimension((int) num_comorbid, (int) num_visits);
  //mat_out.attr("dimnames") = Rcpp::List::create(icd9Mapping.names(), out_row_names);
  valgrindCallgrindStop();
  //return mat_out;
  return Rcpp::wrap(out);
}

