#include <Rcpp.h>
#include "is.h"
#include "manip.h"
#include "manip_alt.h"
#include "util.h"
#include "appendMinor.h"
#include "convert.h"
#include "config.h"

#ifdef HAVE_TESTTHAT_H
#include <testthat.h>

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

#ifdef ICD_DEBUG
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
    expect_error(icd9MajMinToCode(mj, mn, true));

    mn = CV::create("01", "02");
    mj = CV::create("100", "101", "102");
    expect_error(icd9MajMinToCode(mj, mn, true));
  }
}
#endif // end debug-only block

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

context("test alternate zero-adding code") {
  test_that("when a code would be made ambiguous, don't change it") {
    expect_true(Rcpp::as<std::string>(icd9AddLeadingZeroesDirect("E010", true)[0]) == "E010");
  }
  test_that("short E codes work") {
    expect_true(Rcpp::as<std::string>(icd9AddLeadingZeroesDirect("E1", true)[0]) == "E001");
    //Rcpp::String e1 = icd9AddLeadingZeroes_alt_ShortSingle("E1");
    //expect_true(Rcpp::as<std::string>(CV::create(e1)) == "E001");
  }
}

// endif have testthat
#endif
