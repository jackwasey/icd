#include "local.h"
#include "relevant.h"
using namespace Rcpp;

#ifndef MAPPLUS_H_
#define MAPPLUS_H_

class MapPlus {
public:
  MapPlus(const List &icd9Mapping, const Relevant &rh);
  void buildMatrix();
  List map; // consider ListOf<IntegerVector>
  DenseMap mat;
  R_xlen_t rows() { return mat.rows(); }
  R_xlen_t cols() { return mat.cols(); }
};

#endif // MAPPLUS_H
