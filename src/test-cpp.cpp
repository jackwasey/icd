#include <Rcpp.h>
#include "is.h"
#include "util.h"
#include "convert.h"
#include "../inst/include/icd.h"

#ifdef TESTTHAT_HPP
#include <testthat.h>

context("C++ Unit Test") {
  test_that("two plus two is four") {
    int result = 2 + 2;
    expect_true(result == 4);
  }
}

context("test internal 'is' functions") {
  test_that("is a basic") {
    std::vector<std::string> v;
    v.push_back("V10");
    std::vector<bool> result = icd9IsA(v, "V", false);
    for (std::vector<bool>::iterator i = result.begin(); i != result.end(); ++i) {
      expect_true(*i);
    }
  }
}

context("test get OMP max threads") {
  test_that("max threads give a semi-sensible number") {
    int i = getOmpMaxThreads();
    expect_true(i >= 0);
  }
}

context("test get OMP threads") {
  test_that("threads give a semi-sensible number") {
    int i = getOmpThreads();
    expect_true(i >= 0);
  }
}

// context("test randomMajorCpp") {
//   test_that("random majors looks okay") {
//     Rcpp::NumericVector res = randomMajorCpp(5L);
//     expect_true(res.size() == 5);
//     expect_true(all(res >=0));
//   }
// }

context("icd9ShortToPartsCpp") {
  test_that("icd9ShortToPartsCpp gives NA value") {
    Rcpp::List out = icd9ShortToPartsCpp("E12345678", "");

    Rcpp::CharacterVector j = out["major"];
    Rcpp::CharacterVector n = out["minor"];

    expect_true(Rcpp::CharacterVector::is_na(j[0]));
    expect_true(Rcpp::CharacterVector::is_na(n[0]));

  }

  test_that("icd9ShortToPartsCpp multiple inptus gives multiple NA values") {
    Rcpp::CharacterVector cv = Rcpp::CharacterVector::create("E3417824921",
                                                             "E375801347",
                                                             "E8319473422");
    Rcpp::List out = icd9ShortToPartsCpp(cv, "");

    Rcpp::CharacterVector j = out["major"];
    Rcpp::CharacterVector n = out["minor"];

    expect_true(Rcpp::CharacterVector::is_na(j[0]));
    expect_true(Rcpp::CharacterVector::is_na(n[0]));
    expect_true(Rcpp::CharacterVector::is_na(j[1]));
    expect_true(Rcpp::CharacterVector::is_na(n[1]));
    expect_true(Rcpp::CharacterVector::is_na(j[2]));
    expect_true(Rcpp::CharacterVector::is_na(n[2]));

  }
}

// context("Rcpp exports") {
//   test_that("Rcpp exports are valid") {
//
//     int i = icd_RcppExport_validate("icd");
//     expect_that(i > 0);
//   }
// }

context("test parallel debug") {
  test_that("debug parallel runs without error") {
    debug_parallel();
    expect_true(true);
  }
}

context("random data") {
  test_that("random numeric vector of major codes is ok") {
    Rcpp::NumericVector v = randomMajorCpp(10);
    expect_true(v.size() == 10);
    bool gt_zero = is_true(Rcpp::all(v > 0));
    bool lt_thousand = Rcpp::is_true(Rcpp::all(v < 1000));
    expect_true(gt_zero);
    expect_true(lt_thousand);
  }

  test_that("random N codes") {
    std::vector<std::string> n = icd9RandomShortN(1);
    expect_true(n.size() == 1);
    expect_true(n[0].substr(0, 1) != "E");
    expect_true(n[0].substr(0, 1) != "V");

    n = icd9RandomShortN(999);
    expect_true(n.size() == 999);
  }

  test_that("random V codes") {
    std::vector<std::string> v = icd9RandomShortV(1);
    expect_true(v.size() == 1);
    expect_true(v[0].substr(0, 1) == "V");

    v = icd9RandomShortV(999);
    expect_true(v.size() == 999);
    expect_true(v[998].substr(0, 1) == "V");
    expect_true(v[0].size() >= 3);
    expect_true(v[0].size() <= 5);

  }

  test_that("random E codes") {
    std::vector<std::string> e = icd9RandomShortE(1);
    expect_true(e.size() == 1);
    expect_true(e[0].substr(0, 1) == "E");

   e = icd9RandomShortE(999);
   expect_true(e.size() == 999);
   expect_true(e[998].substr(0, 1) == "E");
   expect_true(e[0].size() >= 4);
   expect_true(e[0].size() <= 5);
  }

  test_that("random any code") {
    std::vector<std::string> c = icd9RandomShort(1);
    expect_true(c.size() == 1);
    expect_true(c[0].size() >= 3);
    expect_true(c[0].size() <= 5);
  }

}

context("valgrind hooks") {
  test_that("start callgrind") {
    int i = valgrindCallgrindStart(false);
    expect_true(i == 0);
  }

  test_that("stop callgrind") {
    int i = valgrindCallgrindStop();
    expect_true(i == 0);
  }
}

context("fast int to string") {
  test_that("Rcpp version works") {
    Rcpp::IntegerVector iv;
    iv = Rcpp::IntegerVector::create(1);
    expect_true(Rcpp::as<std::string>(fastIntToStringRcpp(iv)) == "1");

    iv = Rcpp::IntegerVector::create(9);
    expect_true(Rcpp::as<std::string>(fastIntToStringRcpp(iv)) == "9");

    iv = Rcpp::IntegerVector::create(123456);
    expect_true(Rcpp::as<std::string>(fastIntToStringRcpp(iv)) == "123456");

    iv = Rcpp::IntegerVector::create(2, 33, 444, 5555, 66666, 123456);
    Rcpp::CharacterVector cv = Rcpp::CharacterVector::create("2", "33", "444", "5555", "66666", "123456");
    expect_true(Rcpp::is_true(Rcpp::all(fastIntToStringRcpp(iv) == cv)));

      }

// duplicated code...
  test_that("std version works") {
    std::vector<int> iv;
    iv.push_back(1);
    std::vector<std::string> cv;
    cv.push_back("1");
    expect_true(fastIntToStringStd(iv) == cv);

    iv[0] = 9;
    cv[0] = "9";
    expect_true(fastIntToStringStd(iv) == cv);

    iv[0] = 123456;
    cv[0] = "123456";
    expect_true(fastIntToStringStd(iv) == cv);

    iv.push_back(2);
    iv.push_back(33);
    iv.push_back(444);
    iv.push_back(5555);
    iv.push_back(66666);
    cv.push_back("2");
    cv.push_back("33");
    cv.push_back("444");
    cv.push_back("5555");
    cv.push_back("66666");
    expect_true(fastIntToStringStd(iv) == cv);
  }

  }
#endif
