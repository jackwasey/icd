#include "config.h"
#include "icd_types.h"
#include "local.h"
#include "is.h"
#include "manip.h"
#include "util.h"
#include "appendMinor.h"
#include "convert.h"

// nocov start

// nocov because covr seems to misinterpret contents of context sections

#ifdef ICD_CATCH
#include <testthat.h>

using namespace Rcpp;

context("internal 'is' functions") {
  test_that("is n") {
    VecStr v;
    v.push_back("10");
    v.push_back("0");
    v.push_back("10.");
    v.push_back("10.9");
    v.push_back("100.99");
    v.push_back("9999");
    v.push_back("1");
    std::vector<bool> result = icd9_is_n_cpp(v);
    std::vector<bool> result2 = icd9_is_v_cpp(v);
    std::vector<bool> result3 = icd9_is_e_cpp(v);
    for (std::vector<bool>::size_type i = 0; i != result.size(); ++i) {
      expect_true(result[i]);
      expect_false(result2[i]);
      expect_false(result3[i]);
    }
  }

  test_that("is v") {
    VecStr v;
    v.push_back("V10");
    v.push_back("V0");
    v.push_back("V10.");
    v.push_back("V10.9");
    v.push_back("V99.99");
    v.push_back("V9999");
    v.push_back("v1");
    std::vector<bool> result2 = icd9_is_n_cpp(v);
    std::vector<bool> result = icd9_is_v_cpp(v);
    std::vector<bool> result3 = icd9_is_e_cpp(v);
    for (std::vector<bool>::size_type i = 0; i != result.size(); ++i) {
      expect_true(result[i]);
      expect_false(result2[i]);
      expect_false(result3[i]);
    }
  }

  test_that("is e basic") {
    VecStr v;
    v.push_back("E999");
    v.push_back("E0");
    v.push_back("E999.");
    v.push_back("E999.9");
    v.push_back("E9999");
    v.push_back("e000");
    std::vector<bool> result3 = icd9_is_n_cpp(v);
    std::vector<bool> result2 = icd9_is_v_cpp(v);
    std::vector<bool> result = icd9_is_e_cpp(v);
    for (std::vector<bool>::size_type i = 0; i != result.size(); ++i) {
      expect_true(result[i]);
      expect_false(result2[i]);
      expect_false(result3[i]);
    }
  }
}

context("icd9ShortToPartsCpp") {
  test_that("icd9ShortToPartsCpp gives NA value") {
    Rcpp::List out = icd9ShortToPartsCpp("E12345678", "");
    CV j = out["mjr"];
    CV n = out["mnr"];
    expect_true(CV::is_na(j[0]));
    expect_true(CV::is_na(n[0]));
  }

  test_that("icd9ShortToPartsCpp multiple inptus gives multiple NA values") {
    CV cv = CV::create("E3417824921",
                       "E375801347",
                       "E8319473422");
    Rcpp::List out = icd9ShortToPartsCpp(cv, "");
    CV j = out["mjr"];
    CV n = out["mnr"];
    expect_true(CV::is_na(j[0]));
    expect_true(CV::is_na(n[0]));
    expect_true(CV::is_na(j[1]));
    expect_true(CV::is_na(n[1]));
    expect_true(CV::is_na(j[2]));
    expect_true(CV::is_na(n[2]));
  }
}

context("parallel debug") {
  test_that("debug parallel runs without error") {
    debug_parallel_env();
    debug_parallel();
    expect_true(true);
  }
}

context("random data") {
  test_that("random numeric vector of major codes is ok") {
    Rcpp::NumericVector nv = randomMajorCpp(1);
    // minimal test:
    expect_true(nv.size() == 1);
    nv = randomMajorCpp(10);
    expect_true(nv.size() == 10);
    nv = randomMajorCpp(10000);
    expect_true(nv.size() == 10000);
    Rcpp::NumericVector v = randomMajorCpp(10);
    expect_true(v.size() == 10);
    bool gt_zero = is_true(Rcpp::all(v > 0));
    bool lt_thousand = Rcpp::is_true(Rcpp::all(v < 1000));
    expect_true(gt_zero);
    expect_true(lt_thousand);
  }

  test_that("random N codes") {
    VecStr n = icd9RandomShortN(1);
    expect_true(n.size() == 1);
    expect_true(n[0].substr(0, 1) != "E");
    expect_true(n[0].substr(0, 1) != "V");
    n = icd9RandomShortN(999);
    expect_true(n.size() == 999);
  }

  test_that("random V codes") {
    VecStr v = icd9RandomShortV(1);
    expect_true(v.size() == 1);
    expect_true(v[0].substr(0, 1) == "V");
    v = icd9RandomShortV(999);
    expect_true(v.size() == 999);
    expect_true(v[998].substr(0, 1) == "V");
    expect_true(v[0].size() >= 3);
    expect_true(v[0].size() <= 5);
  }

  test_that("random E codes") {
    VecStr e = icd9RandomShortE(1);
    expect_true(e.size() == 1);
    expect_true(e[0].substr(0, 1) == "E");
    e = icd9RandomShortE(999);
    expect_true(e.size() == 999);
    expect_true(e[998].substr(0, 1) == "E");
    expect_true(e[0].size() >= 4);
    expect_true(e[0].size() <= 5);
  }
  test_that("random any code") {
    VecStr c = icd9RandomShort(1);
    expect_true(c.size() == 1);
    expect_true(c[0].size() >= 3);
    expect_true(c[0].size() <= 5);
  }
}

//#ifdef ICD_DEBUG
context("MajMin to code") {
  test_that("differing lengths gives error") {
    CV mj = CV::create("100");
    CV mn = CV::create("01");
    CV rs = CV::create("10001");
    expect_true(Rcpp::is_true(Rcpp::all(icd9MajMinToCode(mj, mn, true) == rs)));
    mj = CV::create("101", "102");
    mn = CV::create("01", "02");
    rs = CV::create("10101", "10202");
    expect_true(Rcpp::is_true(Rcpp::all(icd9MajMinToCode(mj, mn, true) == rs)));
    mj = CV::create("100");
    mn = CV::create("01", "02");
    //expect_error(icd9MajMinToCode(mj, mn, true));
    mn = CV::create("01", "02");
    mj = CV::create("100", "101", "102");
    //expect_error(icd9MajMinToCode(mj, mn, true));
  }
}
//#endif // end debug-only block

context("add leading zeroes to major") {
  test_that("when major len is 0, result is empty") {
    expect_true(icd9AddLeadingZeroesMajorSingleStd("") == "");
  }
  test_that("E code with two char major works") {
    expect_true(icd9AddLeadingZeroesMajorSingleStd("E1") == "E001");
  }
  test_that("E code with three char major works") {
    expect_true(icd9AddLeadingZeroesMajorSingleStd("E01") == "E001");
  }
}

context("refactor") {
  test_that("factor self works") {
    IntegerVector f = {1, 2};
    CV new_levels = CV::create("a", "b");
    f.attr("levels") = new_levels;
    f.attr("class") = "factor";
    IntegerVector res = refactor(f, new_levels);
    expect_true(is_true(all(f == res)));
    bool levels_equal = is_true(all(((CV)f.attr("levels")) == ((CV)res.attr("levels"))));
    expect_true(levels_equal);
  }
  test_that("factor simple re-order") {
    IntegerVector f = {1, 2};
    f.attr("levels") = CV::create("a", "b");
    f.attr("class") = "factor";
    CV new_levels = CV::create("b", "a");
    IntegerVector f2 = {2, 1};
    f2.attr("levels") = new_levels;
    f2.attr("class") = "factor";
    IntegerVector res = refactor(f, new_levels);
    expect_true(is_true(all(f2 == res)));
    bool levels_equal = is_true(all(((CV)f2.attr("levels")) == ((CV)res.attr("levels"))));
    expect_true(levels_equal);
  }
  test_that("factor complex re-order") {
    IntegerVector f = {1, 3, 2};
    f.attr("levels") = CV::create("a", "b", "c");
    f.attr("class") = "factor";
    IntegerVector f2 = {3, 1, 2};
    CV new_levels = CV::create("c", "b", "a");
    f2.attr("levels") = new_levels;
    f2.attr("class") = "factor";
    IntegerVector res = refactor(f, new_levels);
    expect_true(is_true(all(f2 == res)));
    bool levels_equal = is_true(all(((CV)f2.attr("levels")) == ((CV)res.attr("levels"))));
    expect_true(levels_equal);
  }
  test_that("factor complex re-order w extra") {
    IntegerVector f = {1, 3, 2};
    f.attr("levels") = CV::create("a", "b", "c", "d");
    f.attr("class") = "factor";
    IntegerVector f2 = {3, 1, 2};
    CV new_levels = CV::create("c", "b", "a");
    f2.attr("levels") = new_levels;
    f2.attr("class") = "factor";
    IntegerVector res = refactor(f, new_levels);
    expect_true(is_true(all(f2 == res)));
    bool levels_equal = is_true(all(((CV)f2.attr("levels")) == ((CV)res.attr("levels"))));
    expect_true(levels_equal);
  }
  test_that("factor complex re-order w extra and missing") {
    IntegerVector f = {1, 3, 2};
    f.attr("levels") = CV::create("a", "b", "c", "d");
    f.attr("class") = "factor";
    IntegerVector f2 = {NA_INTEGER, 1, 2};
    CV new_levels = CV::create("c", "b", "e");
    f2.attr("levels") = new_levels;
    f2.attr("class") = "factor";
    IntegerVector res = refactor(f, new_levels);
    expect_true(res.size() == 3);
    expect_true(res[0] == f2[0]);
    expect_true(res[1] == f2[1]);
    expect_true(res[2] == f2[2]);
    expect_true(IntegerVector::is_na(res[0]));
    expect_false(IntegerVector::is_na(res[1]));
    expect_false(IntegerVector::is_na(res[2]));
    bool levels_equal = is_true(all(((CV)f2.attr("levels")) == ((CV)res.attr("levels"))));
    expect_true(levels_equal);
    std::string cl = f2.attr("class");
    std::string factor_str = "factor";
    expect_true(cl == factor_str);
    // expect_true(is_true(all(res == f2))); // doesn't work because NA = NA is NA, not true
  }
}

// endif have testthat ICD_CATCH
#endif

// nocov end
