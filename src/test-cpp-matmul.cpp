#ifdef ICD_CATCH
#include "appendMinor.h"
#include "comorbidMatMul.h"
#include "convert.h"
#include "icd_types.h"
#include "is.h"
#include "local.h"
#include "manip.h"
#include "mapplus.h"
#include "refactor.h"
#include "relevant.h"
#include "util.h"

/*
 * Rscript -e
 * "devtools::load_all();devtools::test(filter='cpp',reporter='Location')"
 */

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
  List map       = List::create(_["X"] = v1, _["Y"] = v2);
  List map_v1    = List::create(_["V"] = v1, _["W"] = v1);
  List map_v12   = List::create(_["A"] = CharacterVector::create("V10", "V20"),
                              _["B"] = CharacterVector::create("V12", "V40"));
  DataFrame df1  = DataFrame::create(_["id"]               = CharacterVector::create("1001", "1002"),
                                    _["dx0"]              = v1,
                                    _["dx1"]              = v2,
                                    _["stringsAsFactors"] = false);
  DataFrame df2  = DataFrame::create(_["id"]  = CharacterVector::create("1001", "1002"),
                                    _["dx0"] = v1,
                                    _["dx1"] = v2,
                                    _["dx2"] = CharacterVector::create(NA_STRING, NA_STRING),
                                    _["dx2"] = CharacterVector::create(NA_STRING, "100"),
                                    _["stringsAsFactors"] = false);
  DataFrame dff1 = DataFrame::create(_["id"]  = CharacterVector::create("1001", "1002"),
                                     _["dx0"] = v1,
                                     _["dx1"] = v2,
                                     _["stringsAsFactors"] = true);
  DataFrame dff2 = DataFrame::create(_["id"]  = CharacterVector::create("1001", "1002"),
                                     _["dx0"] = v1,
                                     _["dx1"] = v2,
                                     _["dx2"] = CharacterVector::create(NA_STRING, NA_STRING),
                                     _["dx2"] = CharacterVector::create(NA_STRING, "100"),
                                     _["stringsAsFactors"] = true);
  test_that("make a relevant object with character vectors") {
    Relevant r1(map, v1);
    expect_true(r1.str_codes.size() == v1.size());
    expect_true(r1.hash.size() == v1.size());
    expect_true(r1.keys.size() == v1.size());
    Relevant r2(map, v2);
  }

  test_that("make a relevant object with character vectors") {
    Relevant r0(map_v1, v2);
    expect_true(r0.str_codes.size() == 0);
    expect_true(r0.hash.size() == 0);
    expect_true(r0.keys.size() == 0);
  }

  test_that("make a relevant object with some cols of data frame") {
    Relevant rdf0(map_v1, df1, "dx0");
    Relevant rdf1(map_v1, df1, "dx1");
    Relevant rdf01(map_v1, df1, CV::create("dx0", "dx1"));
    expect_true(rdf0.keys.size() == v1.size());
    expect_true(rdf1.keys.size() == 0);
  }

  test_that("make a relevant object with code factor") {
    IntegerVector f1;
    f1.push_back(2);
    f1.push_back(1);
    f1.attr("levels") = v1;
    f1.attr("class")  = "factor";
    Relevant r(map, f1);
  }

  test_that("make a relevant object with map with matching items split") {
    Relevant r12(map_v12, v1);
    expect_true(r12.str_codes.size() == 1);
    expect_true(r12.hash.size() == 1);
    expect_true(r12.keys.size() == 1);
    Relevant r12b(map_v12, v2);
    expect_true(r12b.str_codes.size() == 1);
    expect_true(r12b.hash.size() == 1);
    expect_true(r12b.keys.size() == 1);
  }

  test_that("make mapplus object") {
    Relevant r1(map, v1);
    Relevant r2(map, v2);
    MapPlus m1(map, r1);
    MapPlus m2(map, r2);
  }

  test_that("do a wide comorbid matrix multiplication") {
    comorbidMatMulWide(df1, map, "id", CharacterVector::create("dx0", "dx1"));
    //  comorbidMatMulWide(dff1, map, "id", CharacterVector::create("dx0",
    //  "dx1"));
  }

  test_that("TEMPORARY! use rel unordered map test lookup") {
    Relevant r12(map, df1, CharacterVector::create("dx0", "dx1"));
    RelMap::iterator got;
    got = r12.rel_map.find("V10");
    expect_true(got != r12.rel_map.end());
    got = r12.rel_map.find("V11");
    expect_true(got != r12.rel_map.end());
    got = r12.rel_map.find("V12");
    expect_true(got != r12.rel_map.end());
    got = r12.rel_map.find("V13");
    expect_true(got != r12.rel_map.end());
  }
}
#endif
