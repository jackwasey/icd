#include "mapplus.h"
#include "comorbidMatMul.h"
#include "fastIntToString.h"
#include "icd_types.h"
#include "local.h"
#include "refactor.h"
#include "relevant.h"
#include "valgrind_icd.h"
#include <cstring>
#include <string>

using namespace Rcpp;

// MapPlus constructor - a reduced comobidity map for one computation
MapPlus::MapPlus(const List &mapList, const Relevant &r) {
  // take a map of character vectors and reduce it to only relevant
  // codes using hashmap
  //
  // downside is that each list element has a copy of the same relevant levels.
  // List remap(const List& map, IHS& relevantHash) {
  CharacterVector cmbs = mapList.names();
  for (R_xlen_t i = 0; i != mapList.size(); ++i) {
    String cmb_name = cmbs[i];
    TRACE("remapping: " << cmb_name.get_cstring());
    if (TYPEOF(mapList[0]) != STRSXP)
      stop("maps should be lists of character vectors, not factors");
    TRACE("character vector in input map");
    CV this_map_cmb = mapList[i];
    // make factor using existing hash, so R-indexed numbers.
    IntegerVector this_cmb  = (IntegerVector)r.hash.lookup(this_map_cmb);
    this_cmb.attr("levels") = (CharacterVector)r.keys;
    this_cmb.attr("class")  = "factor";
    this_cmb                = this_cmb[!is_na(this_cmb)];
    TRACE_VEC(this_cmb);
    map[cmb_name] = this_cmb;
  } // for
  DEBUG("Map reduced. Initializing the Eigen matrix");
  mat = DenseMap(r.keys.size(), mapList.size());
  mat.setZero();
  DEBUG("mat rows: " << mat.rows() << ", cols: " << mat.cols());
  buildMatrix();
  DEBUG("map matrix built");
}

// takes a map of _factors_ produced by remap. These already only contain
// relevant codes, with factors indicies being relevant.
void MapPlus::buildMatrix() {
  TRACE("map SEXP type is: " << TYPEOF(map[0]));
  assert(Rf_isFactor(map[0]));
  for (auto li = map.begin(); li != map.end(); ++li) {
    const auto col = std::distance(map.begin(), li);
    TRACE("working on comorbidity: " << col);
    IntegerVector v(*li);
    for (R_xlen_t vi = 0; vi != v.size(); ++vi) {
      TRACE("cmb: vi=" << vi << " v[vi]=" << v[vi] << " col=" << col);
      if (!IntegerVector::is_na(v[vi])) {
        ICD_ASSIGN(v[vi] - 1, col); // R to C indexing: the factor index is row
      }
    }
  }
  PRINTCORNERMAP(mat);
}
