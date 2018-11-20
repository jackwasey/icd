#include "icd_types.h"
#include "local.h"
#include "is.h"
#include "manip.h"
#include "util.h"
#include "appendMinor.h"
#include "convert.h"
#include "refactor.h"
#include "relevant.h"
#include "mapplus.h"

#ifdef ICD_CATCH
#include <testthat.h>
using namespace Rcpp;
context("matmul cpp") {
  CharacterVector i;
  i.push_back("a");
  i.push_back("b");
  CharacterVector v1;
  v1.push_back("V10");
  v1.push_back("V11");
  CharacterVector v2;
  v2.push_back("V12");
  v2.push_back("V13");
  List map = List::create(_["X"] = v1,
                          _["Y"] = v2);
  List map_v1 = List::create(_["V"] = v1,
                          _["W"] = v1);
  List map_v12 = List::create(
    _["A"] = CharacterVector::create("V10", "V20"),
    _["B"] = CharacterVector::create("V12", "V40")
  );
  test_that("make a relevant object with character vectors") {
    Relevant r1(map, v1);
    expect_true(r1.relevant.size() == v1.size());
    expect_true(r1.hash.size() == v1.size());
    expect_true(r1.keys.size() == v1.size());
    Relevant r2(map, v2);
  }

  test_that("make a relevant object with character vectors") {
    Relevant r0(map_v1, v2);
    expect_true(r0.relevant.size() == 0);
    expect_true(r0.hash.size() == 0);
    expect_true(r0.keys.size() == 0);
  }

  test_that("make a relevant object with some cols of data frame") {
    DataFrame df = DataFrame::create(_["dx0"] = v1, _["dx1"] = v2);
    Relevant rdf0(map_v1, df, "dx0");
    Relevant rdf1(map_v1, df, "dx1");
    Relevant rdf01(map_v1, df, CV::create("dx0", "dx1"));
    expect_true(rdf0.keys.size() == v1.size());
    expect_true(rdf1.keys.size() == 0);
  }

  test_that("make a relevant object with code factor") {
    IntegerVector f1;
    f1.push_back(2);
    f1.push_back(1);
    f1.attr("levels") = v1;
    f1.attr("class") = "factor";
    Relevant r(map, f1); // TODO: not implemented yet, but should be!
  }

  test_that("make a relevant object with map with matching items split") {
    Relevant r12(map_v12, v1);
    expect_true(r12.relevant.size() == 1);
    expect_true(r12.hash.size() == 1);
    expect_true(r12.keys.size() == 1);
    Relevant r12b(map_v12, v2);
    expect_true(r12b.relevant.size() == 1);
    expect_true(r12b.hash.size() == 1);
    expect_true(r12b.keys.size() == 1);
  }

  test_that("make mapplus object") {
    Relevant r1(map, v1);
    Relevant r2(map, v2);
    MapPlus m1(map, r1);
    MapPlus m2(map, r2);
  }
}
#endif
